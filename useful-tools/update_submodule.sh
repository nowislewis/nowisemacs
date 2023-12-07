log_file=/tmp/git_update_log
rm $log_file
touch $log_file

function pull() {
	log_file=/tmp/git_update_log
	PN=$(basename $(pwd))
	last_commit=$(git rev-parse HEAD)
	echo "package:    $PN" >>$log_file
	git reset --hard && git pull
	git log $last_commit...HEAD --no-merges --color --graph --date=format:'%Y-%m-%d %H:%M:%S' --pretty=format:'%Cred%h%Creset -%C(yellow)%d%C(blue) %sC(green)(%cd) %C(bold blue)<%an>%Creset' --abbrev-commit >>$log_file
	echo "\n" >>$log_file
}

export -f pull
git submodule foreach 'pull' && cat $log_file
