#pragma once

#ifndef ERROR_SUCCESS
	#define ERROR_SUCCESS 0
// The operation completed successfully
#endif

#ifndef ERROR_NOT_FOUND
	#define ERROR_NOT_FOUND 1

	#ifndef ERROR_FILE_NOT_FOUND
		#define ERROR_FILE_NOT_FOUND ERROR_NOT_FOUND
	// The system cannot find the file specified.
	#endif

	#ifndef ERROR_PATH_NOT_FOUND
		#define ERROR_PATH_NOT_FOUND ERROR_NOT_FOUND
	// The system cannot find the path specified.
	#endif

	#ifndef ERROR_FILE_EXISTS
		#define ERROR_FILE_EXISTS ERROR_NOT_FOUND
	// The file exists
	#endif

	#ifndef ERROR_ALREADY_EXISTS
		#define ERROR_ALREADY_EXISTS ERROR_NOT_FOUND
	// Cannot create a file when that file already exists
	#endif
#endif

#ifndef ERROR_MEMORY
	#define ERROR_MEMORY 2

	#ifndef ERROR_NOT_ENOUGH_MEMORY
		#define ERROR_NOT_ENOUGH_MEMORY ERROR_MEMORY
	// Not enough memory resources are available to process this command
	#endif

	#ifndef ERROR_OUTOFMEMORY
		#define ERROR_OUTOFMEMORY ERROR_MEMORY
	// Not enough storage is available to complete this operation
	#endif
#endif

#ifndef ERROR_INVALID_DATA
	#define ERROR_INVALID_DATA 3
// The data is invalid
#endif

#ifndef ERROR_INVALID_PARAMETER
	#define ERROR_INVALID_PARAMETER 4
// The parameter (count of parameters) is incorrect
#endif

#ifndef ERROR_NOT_IMPLEMENTED
	#define ERROR_NOT_IMPLEMENTED 5
// This function is not implemented
#endif

#ifndef ERROR_UNSUPPORTED
	#define ERROR_UNSUPPORTED 6
// Unsupported functional
#endif

#ifndef ERROR_UNKNOWN
	#define ERROR_UNKNOWN -1
// Other case
#endif
