MakeCert -r -pe -ss PrivateCertStore -n "CN=ise.sys(test)" pei-ise-sys-test.cer

CertMgr /add pei-ise-sys-test.cer /s /r localMachine root
CertMgr /add pei-ise-sys-test.cer /s /r localMachine trustedpublisher
