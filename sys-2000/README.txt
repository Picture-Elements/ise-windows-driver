
How to make various parts that have to be made by hand...

The certifinicates only need to be made and installed once. After
that, the key may be used multiple times to sign the catalog.

* Make the pei-ise-sys-test.cer like so:

MakeCert -r -pe -ss PrivateCertStore -n "CN=ise.sys(test)" pei-ise-sys-test.cer

* Install the certificate on the local machine like so:

CertMgr /add pei-ise-sys-test.cer /s /r localMachine root
CertMgr /add pei-ise-sys-test.cer /s /r localMachine trustedpublisher

* Make the catalog files with this command sequence:

inf2cat /drv:. /v /os:xp_x86,vista_x86,server2003_x86,server2008_x86,server2008_x64
SignTool sign /v /s PrivateCertStore -n "ise.sys(test)" /t
http://timestamp.verisign.com/scripts/timestamp.dll ise32.cat ise64.cat
