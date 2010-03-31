
rem First sign the .sys files at the lowest level. This is a little bit
rem of overkill, but it verifies the files. This command assumes that the
rem PictureEmements.p12 file has already been imported to the local store.

SignTool sign /v /n "Picture Elements, Inc." /ac GlobalSign_cross_certificate.cer /d "ise.sys" /t http://timestamp.globalsign.com/scripts/timestamp.dll i386\ise.sys amd64\ise.sys

rem Now update the catalog files, with explicit marks for the supported
rem operating system versions.

inf2cat /drv:. /v /os:xp_x86,vista_x86,server2003_x86,server2008_x86,server2008_x64

rem Finally, sign the catalog files themselves.

SignTool sign /v /n "Picture Elements, Inc." /ac GlobalSign_cross_certificate.cer /d "ise.sys" /t http://timestamp.globalsign.com/scripts/timestamp.dll ise32.cat ise64.cat
