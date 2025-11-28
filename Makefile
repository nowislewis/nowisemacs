DRONES_DIR = $(shell git config "borg.drones-directory" || echo "lib")

-include $(DRONES_DIR)/borg/borg.mk

bootstrap-borg:
	@git submodule--helper clone --name borg --path $(DRONES_DIR)/borg \
 --url https://github.com/emacscollective/borg.git
	@cd $(DRONES_DIR)/borg; git symbolic-ref HEAD refs/heads/main
	@cd $(DRONES_DIR)/borg; git reset --hard HEAD
init-org: init-build
	@rm init.elc
init-clean: init-tangle
# 更新到 remote 最新 commit
update:
	./useful-tools/update_submodule.sh
# 初始化下载，更新到 .gitmodules 中指定的 commit
init:
	@git submodule update --init --jobs 32
	@git submodule foreach git reset --hard
# 修改 .gitmodules 后
sync:
	git submodule sync
native-all: $(foreach package,$(notdir $(wildcard lib/*)), native/$(package))
build-all: $(foreach package,$(notdir $(wildcard lib/*)), build/$(package))
