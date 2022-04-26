#!/bin/bash
echo
echo "Install a graphical interface for R?"
echo
echo "	[1] Emacs Speaks Statistics (ESS) http://ess.r-project.org"
echo "	[2] R Commander http://socserv.mcmaster.ca/jfox/Misc/Rcmdr/"
echo "	[3] RKWard https://rkward.kde.org"
echo "	[4] RStudio https://www.rstudio.com"
echo
echo -n "Enter choice [1-4] or do nothing to skip: "
read -t 15 -n 1 INSTALL
echo

KEY="298A3A825C0D65DFD57CBB651716619E084DAB9"
sudo apt install --no-install-recommends software-properties-common dirmngr
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
gpg --show-keys /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc | grep -i $KEY
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
sudo apt update
sudo apt install -y r-base r-base-dev

BIT="i386"
test $(arch) == "x86_64" && BIT="amd64"
test "$INSTALL" == 1 && sudo apt-get install -y elpa-ess
test "$INSTALL" == 2 && sudo apt-get install -y r-cran-rcmdr
test "$INSTALL" == 3 && sudo apt-get install -y rkward
test "$INSTALL" == 4 && (
	rm rstudio*deb
	RSTUDIO=$(wget -q -O - http://www.rstudio.com/products/rstudio/download/ | grep -o -m 1 "http[^\']*$BIT\.deb" | cut -d '"' -f1)
	wget $RSTUDIO
	sudo dpkg -i rstudio*deb
	sudo apt-get check
	sudo apt-get install -y -f
	)
exit 0
