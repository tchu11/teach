#!/bin/bash
echo
echo "Install a graphical interface for R?"
echo
echo "	[1] Emacs Speaks Statistics (ESS) http://ess.r-project.org"
echo "	[2] R Commander http://socserv.mcmaster.ca/jfox/Misc/Rcmdr/"
echo "	[3] RKWard http://rkward.sourceforge.net"
echo "	[4] RStudio http://www.rstudio.com"
echo
echo -n "Enter choice [1-4] or do nothing to skip: "
read -t 15 -n 1 INSTALL
echo
gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
test $? != 0 && gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys E084DAB9
gpg -a --export E084DAB9 | sudo apt-key add -
sudo add-apt-repository "deb http://cran.rstudio.com/bin/linux/ubuntu $(lsb_release -cs)/"
#sudo add-apt-repository ppa:marutter/rrutter
sudo apt-get update
sudo apt-get install -y r-base r-base-dev

BIT="i386"
test $(arch) == "x86_64" && BIT="amd64"
test "$INSTALL" == 1 && sudo apt-get install -y ess
test "$INSTALL" == 2 && sudo apt-get install -y r-cran-rcmdr
test "$INSTALL" == 3 && sudo apt-get install -y rkward
test "$INSTALL" == 4 && (
	rm rstudio*deb
	RSTUDIO=$(wget -q -O - http://www.rstudio.com/products/rstudio/download/ | grep -o -m 1 "http[^\']*$BIT\.deb")
	wget $RSTUDIO
	sudo dpkg -i rstudio*deb
	sudo apt-get check
	sudo apt-get install -y -f
	)
