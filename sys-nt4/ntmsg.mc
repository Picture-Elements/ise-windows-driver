
MessageIdTypedef = NTSTATUS

SeverityNames = (
	Success	     = 0:STATUS_SEVERITY_SUCCESS
	Informational = 1:STATUS_SEVERITY_INFORMATIONAL
	Warning      = 2:STATUS_SEVERITY_WARNING
	Error        = 3:STATUS_SEVERITY_ERROR
)

FacilityNames = (
	UcrDriver = 7:FACILITY_UCR_ERROR_CODE
)


MessageId=1
Facility=UcrDriver
Severity=Informational
SymbolicName=UCR_MSG_ALIVE
Language=English
uCR channel device is alive.
.

MessageId=+1
Facility=UcrDriver
Severity=Warning
SymbolicName=UCR_HTBA_BUG
Language=English
HalTranslateBusAddress failed to translate bus address.
%r%nGuessing seems to work, that's what I'll do.
.
