;;; netron-xwidgets-test.el --- Tests for netron-xwidgets -*- lexical-binding: t; -*-

(require 'ert)
(require 'netron-xwidgets)
(require 'url)

(defun netron-xwidgets-test--write-dummy-onnx (path)
  "Write a synthetic ONNX ModelProto to PATH with 1 small and 1 large tensor."
  (let ((status
         (call-process
          netron-xwidgets-python-program nil nil nil "-c"
          "import struct, sys
def v(n):
    out = bytearray()
    while n > 0x7f:
        out.append((n & 0x7f) | 0x80); n >>= 7
    out.append(n & 0x7f)
    return bytes(out)
def ld(fn, b): return v((fn << 3) | 2) + v(len(b)) + b
def tensor(name, dims, dt, raw):
    m = b''.join(v((1 << 3) | 0) + v(d) for d in dims)
    m += v((2 << 3) | 0) + v(dt) + ld(8, name.encode()) + ld(9, raw)
    return m
small = tensor('reshape_shape', [3], 7, struct.pack('<3q', 1, 64, 112))
large = tensor('conv_weight', [64, 3, 7, 7], 1, b'\\x42' * (64 * 3 * 7 * 7 * 4))
graph = ld(2, b'g') + ld(5, small) + ld(5, large)
model = v((1 << 3) | 0) + v(8) + ld(7, graph)
open(sys.argv[1], 'wb').write(model)
"
          path)))
    (should (eql status 0))))

(ert-deftest netron-xwidgets-slim-onnx-and-mlir-test ()
  "Verify ONNX and MLIR slimming strips large weights and keeps small constants."
  (let* ((tmp-dir (make-temp-file "netron-test-" t))
         (netron-xwidgets-cache-directory (expand-file-name "cache" tmp-dir))
         (netron-xwidgets-slim-threshold 100)
         (onnx-file (expand-file-name "model.onnx" tmp-dir))
         (mlir-file (expand-file-name "model.mlir" tmp-dir)))
    (unwind-protect
        (progn
          (netron-xwidgets-test--write-dummy-onnx onnx-file)
          (let ((staged (car (netron-xwidgets--stage-file onnx-file))))
            (should (< (file-attribute-size (file-attributes staged)) 300))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally staged)
              (should (search-forward "reshape_shape" nil t))
              (goto-char (point-min))
              (should (search-forward "conv_weight" nil t))))
          (with-temp-file mlir-file
            (insert "module {\n"
                    "  %s = arith.constant dense<[1, 64, 112]> : tensor<3xi64>\n"
                    "  %l = arith.constant dense<["
                    (mapconcat #'identity (make-list 120 "0.125") ", ")
                    "]> : tensor<120xf32>\n"
                    "}\n"
                    "{-# dialect_resources: { builtin: { w: \"0x"
                    (apply #'concat (make-list 32 "deadbeef"))
                    "\" } } #-}\n"))
          (let ((staged-mlir (car (netron-xwidgets--stage-file mlir-file))))
            (with-temp-buffer
              (insert-file-contents staged-mlir)
              (should (search-forward "dense<[1, 64, 112]>" nil t))
              (goto-char (point-min))
              (should (search-forward "dense<\"0x00\"> : tensor<120xf32>" nil t))
              (goto-char (point-min))
              (should (search-forward "w: \"0x00000000\"" nil t)))))
      (delete-directory tmp-dir t))))

(ert-deftest netron-xwidgets-server-lifecycle-test ()
  "Verify start/reuse/stop of the single Netron server process."
  (let* ((tmp-dir (make-temp-file "netron-srv-" t))
         (netron-xwidgets-cache-directory (expand-file-name "cache" tmp-dir))
         (netron-xwidgets-slim-threshold 100)
         (netron-xwidgets-use-xwidgets nil)
         (onnx1 (expand-file-name "demo1.onnx" tmp-dir))
         (onnx2 (expand-file-name "demo2.onnx" tmp-dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'browse-url) #'ignore))
          (netron-xwidgets-test--write-dummy-onnx onnx1)
          (netron-xwidgets-test--write-dummy-onnx onnx2)
          (netron-xwidgets-enable)
          (let* ((buf1 (find-file-noselect onnx1))
                 (proc1 netron-xwidgets--server-process)
                 (url1 netron-xwidgets--server-url))
            (should (process-live-p proc1))
            (let ((resp (url-retrieve-synchronously (concat url1 "/data/demo1.onnx") t t 5)))
              (should (buffer-live-p resp))
              (kill-buffer resp))
            ;; Opening a second file reuses the exact same server process
            (find-file-noselect onnx2)
            (should (eq proc1 netron-xwidgets--server-process))
            (let ((resp (url-retrieve-synchronously (concat url1 "/data/demo2.onnx") t t 5)))
              (should (buffer-live-p resp))
              (kill-buffer resp))
            (kill-buffer buf1)
            ;; Explicit stop command kills the server process
            (netron-xwidgets-stop-server)
            (should-not (process-live-p proc1))))
      (netron-xwidgets-stop-server)
      (delete-directory tmp-dir t))))

(provide 'netron-xwidgets-test)
;;; netron-xwidgets-test.el ends here
