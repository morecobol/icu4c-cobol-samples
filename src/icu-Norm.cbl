      *>CBL PGMNAME(MIXED) CALLINT(SYSTEM) NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "ICU-Norm".
      *
      * Sample COBOL program using ICU Normalization APIs
      **
      * The sample includes the following steps:
      * - Read a string from input file "ICU_NORM_Input.txt"
      *         (file in ANSI format)
      * - Convert the string from codepage into UTF-16 format
      * - Perform quick check on the string, to determine if the
      *         string is in NFD (Canonical decomposition)
      *         normalization format.
      * - Normalize the string into canonical composed form
      *         (FCD and decomposed)
      * - Perform quick check on the result string, to determine
      *         if the string is in NFD normalization form
      * - Convert the string from Unicode  into the codepage format
      * - Write the result to output file "ICU_NORM_Output.txt"
      *         (file in ANSI format)
      * - Write debugging information to Display and
      *         log file "ICU_NORM_Log.txt" (file in ANSI format)
      **
      * The following ICU APIs are used:
      *    ucnv_open
      *    ucnv_toUChars
      *    unorm_normalize
      *    unorm_quickCheck
      *    ucnv_fromUChars
      *    ucnv_close
      **
      * This program was developed on a Windows2000 platform.
      *
      * In order to call ICU APIs, the program is compiled with the
      * compiler options CBL PGMNAME(MIXED) CALLINT(SYSTEM) NODYNAM
      *
      * To run the program the directory containing the ICU DLLs (e.g.
      * C:\icu\icu-2.0\bin) must be in the PATH,
      * and the environment variable ICU_DATA must be defined
      * to point to the ICU data directory
      * e.g. SET ICU_DATA="C:\icu\icu-2.0\data\"
      **
      * DATE:      17.01.02
      * Version:   2.0
      *
       AUTHOR.     Eliezer Cesark
      *=================================================================
      *
       ENVIRONMENT DIVISION.
       Input-Output section.
       File-Control.
      * ---------------------- Input File  -----------------
             Select Optional Input-File
                    Assign to "ICU_Norm_Input.txt"
                    File Status is File-Status-Flag
                    Organization is Line Sequential
                    Access Mode is Sequential.
      * ---------------------- Output File  -----------------
             Select Optional Output-File
                    Assign to "ICU_Norm_Output.txt"
                    File Status is File-Status-Flag
                    Organization is Line Sequential
                    Access Mode is Sequential.
      * ---------------------- Debuging File  -----------------
             Select Optional Debug-File
                    Assign to "ICU_Norm_log.txt"
                    File Status is File-Status-Flag
                    Organization is Line Sequential.
      *=================================================================
      *
       DATA DIVISION.
       File section.
       FD Input-File   External
                       Record  varying from 1 to  70 characters.
         01 Input-Record                   pic X(70).
       FD Output-File  External
                       Recording Mode is F
                       Record contains 80 characters.
         01 Output-Record                  pic X(80).
       FD Debug-File   External
                       Record contains 80 characters.
         01 Debug-Record                   pic X(80).
      *
       Working-Storage section.
      *
      *-----------------  Debugging & Error handling Variables ---------
       01  Debug-Write-Flag                pic 9    value 0.
       01  Debug-Write-sw                  pic 9    value 0.
       01  Debug-Display-sw                pic 9    value 0.
       01  Error-Display-sw                pic 9    value 0.
       01  Error-Line                      pic X(45) value
            "Call to ICU failed - error code returned was:".
      *
      * ---------------------- File Variables -----------------
       01  File-Status-Flag                pic X(2).
       01  Main-index                      pic 999 Binary.
       01  Input-Read-Flag                 pic 9    value 0.
       01  Output-Write-Flag               pic 9    value 0.
      * ---------------------- Input / Output Buffers  -----------------
       01  Input-Buffer.
           03  Input-Buffer-String         pic X(70).
             03  Filler Redefines Input-Buffer-String.
               05  Input-Buffer-Array      pic X occurs 70.
       01  Output-Buffer.
           03  Output-Buffer-String        pic X(80).
             03  Filler Redefines Output-Buffer-String.
               05  Output-Buffer-Array     pic X occurs 80.
       01  Debug-Buffer.
           03  Debug-Text                  pic X(45).
           03  Debug-Value                 pic X(35).
           03  Debug-Norm-Text             pic X(45).
      *-------------------- Libraries / DLLs Variables  ----------------
       01  DLL-Name                      pic X(23) value Z"ICUUC20.DLL".
       01  DLL-Handle                      pic 9(9) Binary.
      *-------------------------  API   Variables ----------------------
       01  API-Name                        pic X(80).
       01  API-Pointer                     Usage is Procedure-Pointer.
       01  ICU-VERSION-SUFFIX              pic X(10) value Z"_2_0".
      *
      *======================== ICU (Unicode) Variables  ===============
      *
      *    --- UTF-16 - UNICODE Format - Source buffer -----------------
      *                            UChar *source,
       01  Unicode-Input-Buffer.
           03  UIB-String                  pic X(160).
             03  Filler Redefines UIB-String.
               05  UIB-Array               pic 9(4) Binary occurs  80.
      *    --- UTF-16 - UNICODE Format - Destination buffer ------------
      *                            UChar *result,
       01  Unicode-Output-Buffer.
           03  UOB-String                  pic X(160).
             03  Filler Redefines UOB-String.
               05  UOB-Array               pic  9(4) Binary occurs  80.
       01  Text-Length                     pic S9(9) Binary.
       01  U-Text-Length                   pic S9(9) Binary value   80.
       01  destCapacity                    pic  9(9) Binary value   80.
       01  U-destCapacity                  pic  9(9) Binary value   80.
       01  Unicode-Check-Buffer.
           03  UCB-String                  pic X(160).
             03  Filler Redefines UCB-String.
               05  UCB-Array               pic  9(4) Binary occurs  80.
      *
      *-------------------------- Ported  from unorm.h -----------------
      *
      *typedef enum {
      *  /** No decomposition/composition. @draft ICU 1.8 */
      *  UNORM_NONE = 1,
      *  /** Canonical decomposition. @draft ICU 1.8 */
      *  UNORM_NFD = 2,
      *  /** Compatibility decomposition. @draft ICU 1.8 */
      *  UNORM_NFKD = 3,
      *  /** Canonical decomposition followed by canonical composition.
      *     @draft ICU 1.8 */
      *  UNORM_NFC = 4,
      *  /** Default normalization. @draft ICU 1.8 */
      *  UNORM_DEFAULT = UNORM_NFC,
      *  /** Compatibility decomposition followed by canonical
      *     composition. @draft ICU 1.8 */
      *  UNORM_NFKC =5,
      *  /** "Fast C or D" form. @draft ICU 2.0 */
      *  UNORM_FCD = 6,
      * ...
      *} UNormalizationMode;
         77  UNORM-NONE                    pic 9     Binary value 1.
         77  UNORM-NFD                     pic 9     Binary value 2.
         77  UNORM-NFKD                    pic 9     Binary value 3.
         77  UNORM-NFC                     pic 9     Binary value 4.
      *  --------------------- UNORM.H ---------------------------
      *     typedef enum UNormalizationCheckResult {
      *       UNORM_NO,
      *       UNORM_YES,
      *       UNORM_MAYBE
      *     } UNormalizationCheckResult;
       01  UNormalizationCheckResult       pic S9(9)   Binary value 0.
         88  UNORM-NO                                  value 0.
         88  UNORM-YES                                 value 1.
         88  UNORM-MAYBE                               value 2.
      *
      *---------------  Normalization  API Variables  ------------------
      *             unorm_normalize(
      *                NormalizationMode Mode,
       01  UNormalizationMode              pic 9(9)  Binary Value 0.
      *                int32_t options,
       01  options                         pic 9(9)  Binary.
      *                int32_t resultLength,
       01  resultLength                    pic 9(9)  Binary.
      *-------------------  Conversion  Variables ----------------------
       01  CODEPAGE-Name                   pic X(32).
       01  Converter-Pointer               pic S9(9) Binary.
      *
      *-------------- Ported from utypes.h -----------------------------
      *
      *                                 UErrorCode *status);
       01  UErrorCode                      pic S9(9) Binary value 0.
      *     Start of information results (semantiCally successful)
         88  U-ERROR-INFO-START                       value -128.
      *     A resource bundle lookup returned a fallback result
      *     (not an error)
         88  U-USING-FALLBACK-ERROR                   value -128.
      *     A resource bundle lookup returned a result from the root
      *      locale (not an error)
         88  U-USING-DEFAULT-ERROR                    value -127.
      *     A SafeClone operation required allocating memory
      *     (informational only)
         88  U-SAFECLONE-ALLOCATED-ERROR              value -126.
      *     This must always be the last warning value to indicate the
      *      limit for UErrorCode warnings (last warning code +1)
         88  U-ERROR-INFO-LIMIT                       value  0.
      *     No error. no warning.
         88  U-ZERO-ERROR                             value  0.
      *     Start of codes indicating failure
         88  U-ILLEGAL-ARGUMENT-ERROR                 value  1.
         88  U-MISSING-RESOURCE-ERROR                 value  2.
         88  U-INVALID-FORMAT-ERROR                   value  3.
         88  U-FILE-ACCESS-ERROR                      value  4.
      *     Indicates a bug in the library code
         88  U-INTERNAL-PROGRAM-ERROR                 value  5.
         88  U-MESSAGE-PARSE-ERROR                    value  6.
      *     Memory allocation error
         88  U-MEMORY-ALLOCATION-ERROR                value  7.
         88  U-INDEX-OUTOFBOUNDS-ERROR                value  8.
      *     Equivalent to Java ParseException
         88  U-PARSE-ERROR                            value  9.
      *     In the Character conversion routines:
      *     Invalid character or sequence was encountered
         88  U-INVALID-CHAR-FOUND                     value 10.
      *     In the Character conversion routines:
      *     More bytes are required to complete the conversion
      *     successfully
         88  U-TRUNCATED-CHAR-FOUND                   value 11.
      *     In codeset conversion: a sequence that does NOT belong in
      *      the codepage has been encountered
         88  U-ILLEGAL-CHAR-FOUND                     value 12.
      *     Conversion table file found. but corrupted
         88  U-INVALID-TABLE-FORMAT                   value 13.
      *     Conversion table file not found
         88  U-INVALID-TABLE-FILE                     value 14.
      *     A result would not fit in the supplied Buffer
         88  U-BUFFER-OVERFLOW-ERROR                  value 15.
      *     Requested operation not supported in current context
         88  U-UNSUPPORTED-ERROR                      value 16.
      *     an operation is requested over a resource
      *     that does not support it
         88  U-RESOURCE-TYPE-MISMATCH                 value 17.
      *     ISO-2022 illegal escape sequence
         88  U-ILLEGAL-ESCAPE-SEQUENCE                value 18.
      *     ISO-2022 unsupported escape sequence
         88  U-UNSUPPORTED-ESCAPE-SEQUENCE            value 19.
      *     No space available for in-Buffer expansion for Arabic
      *     shaping
         88  U-NO-SPACE-AVAILABLE                     value 20.
      *     This must always be the last value to indicate the limit for
      *     UErrorCode (last error code +1)
         88  U-ERROR-LIMIT                            value 21.
      *     U-SUCCESS and U-FAILURE as 88s instead of C macros
         88  U-SUCCESS                             values -128 thru 0.
         88  U-FAILURE                             values 1 thru 21.
      *
      *=================================================================
      *
       Linkage section.
      *
      *====================== Main Program  ============================
      *
       PROCEDURE DIVISION.
       MAIN section.
      *
      *---------------------- Set Online debugging ---------------------
      *
           Move    1 to Debug-Display-sw.
           Move    1 to Debug-Write-Sw.
           Perform Debug-Open-sec.
           Move ">>> Program: ICU-Norm - ver 2.0 - 20.01.02"
                                                   to Debug-Text.
           Perform Debug-Display-sec.
      *
           Perform Input-Open-sec.
           Perform Output-Open-sec.
      *
      *==========================   ICU Normalization  =================
      *
           Perform Load-DLL-sec.
           Perform Open-Unicode-Converter-sec.
           IF U-SUCCESS
             Perform   Normalization-Main-Loop-sec
                       Varying Main-index  FROM 1 by 1
                       until               Input-Read-Flag = 0
             Perform   Close-Unicode-Converter-sec.
      *
      *----------------- Close and Free buffers ------------------------
      *
           Perform Free-DLL-sec.
           Perform Input-Close-sec.
           Perform Output-Close-sec.
           Perform Debug-Close-sec.
      *
           Stop Run.
      *
      *================= ICU Normalization Main Loop ===================
      *
       Normalization-Main-Loop-sec  section.
       Normalization-Main-Loop.
             Perform   Input-Read-sec.
             IF    Input-Read-Flag = 1 then
               Perform   Convert-to-Unicode-sec
               Perform   Normalization-sec
               Perform   Convert-from-Unicode-sec
               Perform   Output-Write-sec
             End-if.
       Normalization-Main-Loop-ex.
           Exit.
      *
      *================= Load/Free ICU Libraries (Call "LoadLibraryA") =
      *
       Load-DLL-sec section.
       Load-DLL.
           Move    "Call :    LoadLibraryA" to Debug-Text.
           Move    DLL-Name                 to Debug-Value.
           Perform Debug-Display-sec.
      *
           Call    "LoadLibraryA"  using by reference  DLL-Name
                                   Returning           DLL-Handle.
           IF DLL-Handle = ZEROS
              Move     "Couldn't load "    to Debug-Text
              Move     DLL-Name            to Debug-Value
              Move     1                   to Error-Display-sw
              Perform  Debug-Display-sec
              Move
            "Check that your PATH includes the directory with the DLLs."
                                           to Debug-Buffer
              Move     1                   to Error-Display-sw
              Perform  Debug-Display-sec
              Stop Run.
       Load-DLL-ex.
           Exit.
      *
       Free-DLL-sec section.
       Free-DLL.
           Move    "Call :    FreeLibrary" to Debug-Text.
           Move    DLL-Name                to Debug-Value
           Perform Debug-Display-sec
           Call    "FreeLibrary"        using DLL-Handle.
       Free-DLL-ex.
           Exit.
      *
      *==================== Open API in DLL (Call "GetProcAddress") ====
      *
       Get-API-Pointer-sec section.
       Get-API-Pointer.
           Move    "Call API Function :"   to Debug-Text.
           Move    API-Name                to Debug-Value.
           Perform Debug-Display-sec.
           Call    "GetProcAddress" using  by value     DLL-Handle
                                           by reference API-Name
                                           Returning    API-Pointer.
           IF API-Pointer = NULL
              Move     "The following API was not found in DLL:"
                                           to Debug-Text
              Move     API-Name            to Debug-Value
              Move     1                   to Error-Display-sw
              Perform  Debug-Display-sec
              Stop Run.
       Get-API-Pointer-ex.
           Exit.
      *
       Check-Call-to-API-sec section.
       Check-Call-to-API.
           IF U-FAILURE
              Move     Error-Line  to  Debug-Text
              Move     UErrorCode  to  Debug-Value
              Move     1           to  Error-Display-sw
              Perform  Debug-Display-sec
              IF U-BUFFER-OVERFLOW-ERROR
                 Move
           "BUF-OVER-ERR: A result would not fit in the supplied Buf."
                               to  Debug-Buffer
                 Move     1    to  Error-Display-sw
                 Perform  Debug-Display-sec
              End-if
              IF U-FILE-ACCESS-ERROR
                 Move
           "Check that the environment variable ICU_DATA is defined:"
                               to  Debug-Buffer
                 Move     1    to  Error-Display-sw
                 Perform  Debug-Display-sec
                 Move
           "  SET ICU_DATA=C:\ICU\ICU-2.0\DATA\ - see ReadMe"
                               to  Debug-Buffer
                 Move     1    to  Error-Display-sw
                 Perform  Debug-Display-sec
              End-if.
           Move        0   to  UErrorCode.
       Check-Call-to-API-ex.
           Exit.
      *
      *====================== Convert to Unicode APIs  =================
      *
       Open-Unicode-Converter-sec section.
       Open-Unicode-Converter.
      *   U_CAPI UConverter *U_EXPORT2  ucnv_open(
      *                         const char * ConverterName,
      *                         UErrorCode * err )
      *   Creates a UConverter object with the names
      *         specified as a C String.
           STRING "ucnv_open" ICU-VERSION-SUFFIX
               delimited by size into API-Name.
           Perform Get-API-Pointer-sec.
      *
           Move Z"UTF-8" to CODEPAGE-Name.
           Call API-Pointer using by reference CODEPAGE-Name
                                  by reference UErrorCode
                                  Returning Converter-Pointer.
           Perform Check-Call-to-API-sec.
       Open-Unicode-Converter-ex.
           Exit.
      *
       Convert-to-Unicode-sec section.
       Convert-to-Unicode.
      *  --------------------- UCNV.H ---------------------------
      *   U_CAPI int32_t U_EXPORT2  ucnv_toUChars(
      *                         UConverter * cnv,
      *                         UChar * dest,
      *                         int32_t destCapacity,
      *                         const char * src,
      *                         int32_t srcLength,
      *                         UErrorCode * pErrorCode )
      *
      *   Converts the codepage string into a UTF-16 string using
      *         an existing UConverter.
      *
           STRING "ucnv_toUChars" ICU-VERSION-SUFFIX
               delimited by size into API-Name.
           Perform Get-API-Pointer-sec.
           Call API-Pointer using by value     Converter-Pointer
                                  by reference Unicode-Input-Buffer
                                  by value     U-destCapacity
                                  by reference Input-Buffer
                                  by value     Text-Length
                                  by reference UErrorCode
                                  Returning    U-Text-Length.
           Perform Check-Call-to-API-sec.
       Convert-to-Unicode-ex.
           Exit.
      *
      *====================== Normalization APIs  ======================
      *
       Normalization-sec section.
       Normalization.
      *  --------------------- UNORM.H ---------------------------
      *   U_CAPI int32_t U_EXPORT2  unorm_normalize(
      *                         const UChar * source,
      *                         int32_t sourceLength,
      *                         UNormalizationMode Mode,
      *                         int32_t options,
      *                         UChar * result,
      *                         int32_t resultLength,
      *                         UErrorCode * status )
      *   Normalizes the string according to the specified
      *         normalization mode
      *   Returns: total buffer size needed; if greater than
      *     resultLength, the output was truncated, and the error code
      *     is set to U_BUFFER_OVERFLOW_ERROR.
      *
      *    ------- Check normalization for UNORM-NFD ------
           Move UNORM-NFD          to          UNormalizationMode.
           Move "unorm_quickCheck: Befor Norm. to UNORM-NFD:"
                                   to          Debug-Norm-Text.
           Move Unicode-Input-Buffer to Unicode-Check-Buffer.
           Perform Normalization-Check-sec.
      *
           STRING "unorm_normalize" ICU-VERSION-SUFFIX
               delimited by size into API-Name.
           Perform Get-API-Pointer-sec.
           Move 0          to options.
      *         0          no options defined
           Move UNORM-NFD      to UNormalizationMode.
           Call API-Pointer using by reference Unicode-Input-Buffer
                                  by value     U-Text-Length
                                  by value     UNormalizationMode
                                  by value     options
                                  by reference Unicode-Output-Buffer
                                  by value     U-destCapacity
                                  by reference UErrorCode
                                  Returning    U-Text-Length.
           Perform Check-Call-to-API-sec.
      *    ------- Check normalization for UNORM-NFD ------
           Move UNORM-NFD          to          UNormalizationMode.
           Move "unorm_quickCheck: After Norm. to UNORM-NFD:"
                                   to          Debug-Norm-Text.
           Move Unicode-Output-Buffer to Unicode-Check-Buffer.
           Perform Normalization-Check-sec.
       Normalization-ex.
           Exit.
      *
       Normalization-Check-sec section.
       Normalization-Check.
      *  --------------------- UNORM.H ---------------------------
      *   U_CAPI UNormalizationCheckResult U_EXPORT2  unorm_quickCheck(
      *                         const UChar * source,
      *                         int32_t sourceLength,
      *                         UNormalizationMode Mode,
      *                         UErrorCode * status )
      *
      *   Performs quick check on a String, to quickly determine
      *         if the string is in a particular normalization format
      *
           STRING "unorm_quickCheck" ICU-VERSION-SUFFIX
               delimited by size into API-Name.
           Perform Get-API-Pointer-sec.
           Call API-Pointer
                          using by reference Unicode-Check-Buffer
                                by value     U-Text-Length
                                by value     UNormalizationMode
                                by reference UErrorCode
                                Returning    UNormalizationCheckResult.
           Perform Check-Call-to-API-sec.
      *    ------------------- Write Check Result to Debug -----------
      *  --------------------- UNORM.H ---------------------------
      *     typedef enum UNormalizationCheckResult {
      *       UNORM_NO,
      *       UNORM_YES,
      *       UNORM_MAYBE
      *     } UNormalizationCheckResult;
      *
           Move Debug-Norm-Text    to      Debug-Text.
           IF UNORM-NO
             Move      "NO"        to      Debug-Value.
           IF UNORM-YES
             Move      "YES"       to      Debug-Value.
           IF UNORM-MAYBE
             Move      "MAYBE"     to      Debug-Value.
           Perform  Debug-Display-sec.
       Normalization-Check-ex.
           Exit.
      *
      *====================== Convert from Unicode APIs  ===============
      *
       Convert-from-Unicode-sec section.
       Convert-from-Unicode.
      *  --------------------- UCNV.H ---------------------------
      *   U_CAPI int32_t U_EXPORT2  ucnv_fromUChars(
      *                         UConverter * cnv,
      *                         char * dest,
      *                         int32_t destCapacity,
      *                         const UChar *src,
      *                         int32_t srcLength,
      *                         UErrorCode * pErrorCode )
      *   Converts the Unicode string into a codepage string using
      *         an existing UConverter
           IF U-SUCCESS
              Move SPACES to Output-Buffer
              STRING "ucnv_fromUChars" ICU-VERSION-SUFFIX
                  delimited by size into API-Name
              Perform Get-API-Pointer-sec
              Call API-Pointer
                          using by value      Converter-Pointer
                                by reference  Output-Buffer
                                by value      destCapacity
                                by reference  Unicode-Output-Buffer
                                by value      U-Text-Length
                                by reference  UErrorCode
                                Returning     Text-Length
              Perform Check-Call-to-API-sec.
       Convert-from-Unicode-ex.
           Exit.
      *
       Close-Unicode-Converter-sec section.
       Close-Unicode-Converter.
      *  --------------------- UCNV.H ---------------------------
      *   U_CAPI void  U_EXPORT2  ucnv_close( UConverter * converter )
      *   Deletes the unicode converter and releases associated resources
           STRING "ucnv_close" ICU-VERSION-SUFFIX
               delimited by size into API-Name.
           Perform Get-API-Pointer-sec.
           Call API-Pointer using by value Converter-Pointer.
           Perform Check-Call-to-API-sec.
       Close-Unicode-Converter-ex.
           Exit.
      *
      *========================  Input File  =====================
      *
       Input-Open-sec section.
       Input-Open.
           Move    ZERO        to      File-Status-Flag.
           Open    Input               Input-File.
           IF (File-Status-Flag = "00")  Then
             Move     1                to Input-Read-Flag
           Else
             Move     0                to Input-Read-Flag
             Move "Input File Open Error - code: "
                                       to Debug-Text
             Move     File-Status-Flag to Debug-Value
             Move     1                to Error-Display-sw
             Perform  Debug-Display-sec
             Stop Run
           End-if.
       Input-Open-ex.
           Exit.
      *
      *---------------------   Read the record  -----------------------
       Input-Read-sec section.
       Input-Read.
           IF Input-Read-Flag = 1  Then
             Read  Input-File  into           Input-Buffer
                 at End        Move 0      to Input-Read-Flag.
           IF Input-Read-Flag = 1  Then
             Compute Text-Length = Function Length (Input-Buffer)
             Move    "Input Record  --------------:" to Debug-Text
             Perform Debug-Display-sec
             Move    Input-Buffer          to Debug-Buffer
             Perform Debug-Display-sec
             Move    "-----------------------------" to    Debug-Text
             Perform Debug-Display-sec
           Else
             Move    ZERO                  to Text-Length
           End-if.
       Input-Read-ex.
           Exit.
      *
      *-----------------------  Close the file  ------------------------
       Input-Close-sec section.
       Input-Close.
           Close Input-File.
       Input-Close-ex.
           Exit.
      *
      *========================  Output File  =====================
      *
       Output-Open-sec section.
       Output-Open.
           Move ZERO to File-Status-Flag.
           Open Output Output-File.
           IF (File-Status-Flag = "00")  Then
             Move     1                to Output-Write-Flag
           Else
             Move     0                to Output-Write-Flag
             Move "Output File Open Error - code: "
                                       to Debug-Text
             Move     File-Status-Flag to Debug-Value
             Move     1                to Error-Display-sw
             Perform  Debug-Display-sec
           End-if.
       Output-Open-ex.
           Exit.
      *
      *---------------------   Write the record  -----------------------
       Output-Write-sec section.
       Output-Write.
           IF Output-Write-Flag = 1    Then
             Write  Output-Record          from    Output-Buffer
             Move    "Output Record  -------------:" to    Debug-Text
             Perform Debug-Display-sec
             Move    Output-Buffer           to    Debug-Buffer
             Perform Debug-Display-sec
             Move    "-----------------------------" to    Debug-Text
             Perform Debug-Display-sec
           End-if.
       Output-Write-ex.
           Exit.
      *
      *-----------------------  Close the file  ------------------------
       Output-Close-sec section.
       Output-Close.
           Close       Output-File.
       Output-Close-ex.
           Exit.
      *
      *=================      Debuging  Sesions   ======================
      *
      *--------------------  Open Debug Log File  ----------------------
       Debug-Open-sec section.
       Debug-Open.
           Move spaces to Debug-Buffer.
           Move zero to File-Status-Flag.
           Open Output Debug-File.
           IF  (File-Status-Flag  Not = "00")  Then
             Move 0 to Debug-Write-sw
             Display "Log File Open Error - code: "
                      File-Status-Flag
           End-if.
       Debug-Open-ex.
           Exit.
      *--------------------  Close Debug Log File  ---------------------
       Debug-Close-sec section.
       Debug-Close.
            Close Debug-File.
       Debug-Close-ex.
           Exit.
      *-----------------  Write / Display Debug Information  -----------
       Debug-Display-sec section.
       Debug-Display.
      *    Truncate output at first null character
           INSPECT Debug-Value replacing characters by Spaces
               after initial X"00" first X"00" by Space.
           IF Debug-Display-sw = 1 Or Error-Display-sw = 1
             Move 0 to Error-Display-sw
             Display Debug-Buffer.
           IF  Debug-Write-sw = 1  Then
             Write Debug-Record from Debug-Buffer
           End-if.
           Move " " to Debug-Value.
       Debug-Display-ex.
           Exit.
