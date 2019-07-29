# template script to copy a file (with history) from one git repo to another
# taken from http://blog.neutrino.es/2012/git-copy-a-file-or-directory-from-another-repository-preserving-history/

mkdir /tmp/mergepatchs
cd ~/repo/org
export reposrc=myfile.c #or mydir
git format-patch -o /tmp/mergepatchs $(git log $reposrc|grep ^commit|tail -1|awk '{print $2}')^..HEAD $reposrc
cd ~/repo/dest
git am /tmp/mergepatchs/*.patch
