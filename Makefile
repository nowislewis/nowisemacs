DRONES_DIR = $(shell git config "borg.drones-directory" || echo "lib")

-include $(DRONES_DIR)/borg/borg.mk

bootstrap-borg:
	@git submodule--helper clone --name borg --path $(DRONES_DIR)/borg \
 --url https://github.com/emacscollective/borg.git
	@cd $(DRONES_DIR)/borg; git symbolic-ref HEAD refs/heads/master
	@cd $(DRONES_DIR)/borg; git reset --hard HEAD
clean-init: init.el
	@rm -f init.elc $(INIT_FILES:.el=.elc)
# 更新到 remote 最新 commit
update:
	git submodule update --init --recursive --remote

# 初始化下载，更新到 .gitmodules 中指定的 commit
init:
	git submodule update --init

# 修改 .gitmodules 后
sync:
	git submodule sync
