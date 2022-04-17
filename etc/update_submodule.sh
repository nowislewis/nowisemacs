
FN=/tmp/emacs-update-sessions

function pull() {
     FN=/tmp/emacs-update-sessions
     touch $FN
     PN=$(basename $(pwd))
     CNT=$(grep -c $PN /tmp/emacs-update-sessions)
     echo $PN $CNT
     if [ $CNT -ge 1 ]; then
	echo skip
     else
        git checkout main || git checkout master; git pull && echo $PN >> $FN;
     fi 
}

export -f pull

git submodule foreach 'pull' && rm $FN
