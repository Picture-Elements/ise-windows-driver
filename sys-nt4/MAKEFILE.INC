
# This file is used by the NT DDK to build the message file.

ntmsg.h ntmsg.rc msg00001.bin: ntmsg.mc
    mc -v ntmsg.mc

