
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
Severity=Warning
SymbolicName=ISE_NO_DEVICE
Language=English
Device type not supported by driver.
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

MessageId=10
Facility=ISE
Severity=Warning
SymbolicName=ISE_FRAME_MAP_FAILED
Language=English
Unable to map requested frame.
.

MessageId=11
Facility=ISE
Severity=Informational
SymbolicName=ISE_DETECTED_EJSE
Language=English
Detected EJSE board
.

MessageId=12
Facility=ISE
Severity=Warning
SymbolicName=ISE_GET_ADAPTER_FAILED
Language=English
Unable to allocate DMA Adapter for buffer mapping
.

MessageId=13
Facility=ISE
Severity=Warning
SymbolicName=ISE_GET_ROOT_TABLE_FAILED
Language=English
Unable to allocate DMA Adapter for buffer mapping
.

MessageId=14
Facility=ISE
Severity=Warning
SymbolicName=ISE_NO_RESOURCE_MAP
Language=English
System couldn't map resources to the board.
.

