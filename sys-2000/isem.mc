
MessageIdTypedef = NTSTATUS

SeverityNames = (
	Success       = 0:STATUS_SEVERITY_SUCCESS
	Informational = 1:STATUS_SEVERITY_INFORMATIONAL
	Warning       = 2:STATUS_SEVERITY_WARNING
	Error         = 3:STATUS_SEVERITY_ERROR
)

FacilityNames = (
	ISE = 7:FACILITY_ISE_ERROR_CODE
)

MessageId=1
Facility=ISE
Severity=Informational
SymbolicName=ISE_NO_DEVICES
Language=English
No devices found.
.

MessageId=2
Facility=ISE
Severity=Warning
SymbolicName=ISE_FRAME_UNMAP_FAILED
Language=English
Tried to unmap frame that does not belong to me.
.

MessageId=3
Facility=ISE
Severity=Informational
SymbolicName=ISE_FRAME_UNMAP_ON_CLOSE
Language=English
Implicitly unmapped frame on close.
.

MessageId=4
Facility=ISE
Severity=Warning
SymbolicName=ISE_FRAME_DANGLE_ON_CLOSE
Language=English
Left frame dangling on close
.

MessageId=5
Facility=ISE
Severity=Informational
SymbolicName=ISE_DETECTED_ISE
Language=English
Detected ISE/SSE board
.

MessageId=6
Facility=ISE
Severity=Informational
SymbolicName=ISE_DETECTED_JSE
Language=English
Detected JSE board
.

MessageId=7
Facility=ISE
Severity=Informational
SymbolicName=ISE_TRACE_POINT
Language=English
Debug Trace Point
.

MessageId=8
Facility=ISE
Severity=Warning
SymbolicName=ISE_ROOT_CALLBACK_OVERRUN
Language=English
Internal Driver error, root callback overrun.
.

MessageId=9
Facility=ISE
Severity=Warning
SymbolicName=ISE_ROOT_TIMEOUT
Language=English
Timeout sending root table to the board.
.
