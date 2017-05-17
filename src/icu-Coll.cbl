       IDENTIFICATION DIVISION.
       PROGRAM-ID. "ICU-Coll".
      *>>
      *>> Sample COBOL program using ICU Collation APIs
      *>>
      *> The sample includes the following steps:
      *> - Read a string array from Input file "ICU_Coll_Input.txt"
      *>         (file in ANSI format)
      *> - Convert string array from codepage into UTF-16 format
      *> - Compare the string array into the canonical composed
      *> - Perform bubble sort of string array, according
      *>         to Unicode string equivalence comparisons
      *> - Convert string array from Unicode into codepage format
      *> - Write the result to output file "ICU_Coll_Output.txt"
      *>         (file in ANSI format)
      *> - Write debugging information to Display and
      *>         log file "ICU_Coll_Log.txt" (file in ANSI format)
      *>*>
      *> The following ICU APIs are used:
      *>    ucol_open
      *>    ucol_strcoll
      *>    ucol_close
      *>    ucnv_open
      *>    ucnv_toUChars
      *>    ucnv_fromUChars
      *>    ucnv_close
      *>*>
      *> This program was developed on a Windows2000 platform.
      *>
      *> In order to call ICU APIs, the program is compiled with the
      *> options CBL PGMNAME(MIXED) CALLINT(SYSTEM) NODYNAM
      *>
      *> To run the program the directory containing the ICU DLLs (e.g.
      *> C:\icu\icu-2.0\bin) must be in the PATH,
      *> and the environment variable ICU_DATA must be defined
      *> to point to the ICU data directory
      *> e.g. SET ICU_DATA="C:\icu\icu-2.0\data\"
      *>*>
      *> DATE:      17.01.02
      *> Version:   2.0
      *>
       AUTHOR.     Eliezer Cesark
      *>=================================================================
      *>
      ENVIRONMENT DIVISION.
      Input-Output section.
      File-Control.
      *> ---------------------- Input File  -----------------
             Select Optional Input-File
                    Assign to "ICU_Coll_Input.txt"
                    File Status is File-Status-Flag
                    Organization is Line Sequential
                    Access Mode is Sequential.
      *> ---------------------- Output File  -----------------
             Select Optional Output-File
                    Assign to "ICU_Coll_Output.txt"
                    File Status is File-Status-Flag
                    Organization is Line Sequential
                    Access Mode is Sequential.
      *> ---------------------- Debuging File  -----------------
             Select Optional Debug-File
                    Assign to "ICU_Coll_log.txt"
                    File Status is File-Status-Flag
                    Organization is Line Sequential.
      *>=================================================================
      *>
       DATA DIVISION.
       File section.
       FD Input-File   External
                       Record  varying from 1 to  79 characters.
         01 Input-Record                   pic X(79).
       FD Output-File  External
                       Recording Mode is F
                       Record contains 80 characters.
         01 Output-Record                  pic X(80).
       FD Debug-File   External
                       Record contains 80 characters.
         01 Debug-Record                   pic X(80).
      *>
       Working-Storage section.
      *>
      *>-----------------  Debugging & Error handling Variables ---------
       01  Debug-Write-Flag                pic 9.
       01  Debug-Write-sw                  pic 9.
       01  Debug-Display-sw                pic 9    value 0.
       01  Error-Display-sw                pic 9    value 0.
       01  Error-Line                      pic X(45) value
            "Call to ICU failed - error code returned was:".
      *>
      *> ---------------------- File Variables -----------------
       01  File-Status-Flag                pic X(2).
       01  Main-index                      pic 999 Binary.
       01  Sort-index                      pic 999 Binary.
       01  Sort-Count                      pic 999 Binary.
       01  Sort-Do-sw                      pic 9.
       01  Input-Read-Flag                 pic 9    value 0.
       01  Output-Write-Flag               pic 9    value 0.
       01  Input-Record-Number             pic 999 Binary value 0.
       01  Index-First                     pic 999 Binary value 0.
       01  Index-Second                    pic 999 Binary value 0.
      *> ---------------------- Input / Output Buffers  -----------------
       01  Input-Buffer.
           03  Input-Buffer-String         pic X(79).
             03  Filler Redefines Input-Buffer-String.
               05  Input-Buffer-Array      pic X occurs 79.
       01  Output-Buffer.
           03  Output-Buffer-String        pic X(80).
             03  Filler Redefines Output-Buffer-String.
               05  Output-Buffer-Array     pic X occurs 80.
       01  Debug-Buffer.
           03  Debug-Text                  pic X(45).
           03  Debug-Value                 pic X(35).
      *>-------------------- Libraries / DLLs Variables  ----------------
       01  DLL-Conv-Name                 pic X(23) value Z"ICUUC20.DLL".
       01  DLL-Coll-Name                 pic X(23) value Z"ICUIN20.DLL".
       01  DLL-Name                        pic X(23).
       01  DLL-Handle                      pic 9(9) Binary.
       01  DLL-Coll-Handle                 pic 9(9) Binary.
      *>-------------------------  APIs  Variables ----------------------
       01  API-Name                        pic X(80).
       01  API-Pointer                     Usage is Procedure-Pointer.
       01  ICU-VERSION-SUFFIX              pic X(10) value Z"_2_0".
      *>
      *>======================== ICU (Unicode) Variables  ===============
      *>
      *>    --- UTF-16 - UNICODE Format - Source buffer -----------------
      *>                            UChar *>source,
       01  Unicode-Input-Buffer.
           03  UIB-String                  pic X(160).
             03  Filler Redefines UIB-String.
               05  UIB-Array               pic 9(4) Binary occurs 80.
      *>    --- UTF-16 - UNICODE Format - Destination buffer ------------
      *>                            UChar *>result,
       01  Unicode-Output-Buffer.
           03  UOB-String                  pic X(160).
             03  Filler Redefines UOB-String.
               05  UOB-Array               pic 9(4)  Binary occurs 80.
       01  Unicode-Buffer-Temp.
           03  Unicode-Buffer-Array        pic X(160) occurs 20.
       01  Text-Length                     pic S9(9) Binary.
       01  U-Text-Length                   pic S9(9) Binary value   80.
       01  destCapacity                    pic  9(9) Binary value   80.
       01  U-destCapacity                  pic  9(9) Binary value   80.
      *>--------------------  Conversion Variables ----------------------
       01  toConverterName                 pic X(32).
       01  fromConverterName               pic X(32).
       01  Converter-Name                  pic X(32).
       01  Converter-Name-Link-Pointer     Usage is Pointer.
       01  Converter-Pointer               pic S9(9) Binary.
       01  Converter-toU-Pointer           pic S9(9) Binary.
       01  Converter-fromU-Pointer         pic S9(9) Binary.
       01  Converters-Counter              pic S9(9) Binary.
       01  Converters-Index                pic S9(9) Binary.
      *>--------------------  Collation Variables ----------------------
       01  Collator-Loc                    pic X(32) value Z" ".
       01  Collator-Pointer                pic S9(9) Binary.
      *>-------------- Ported from convrtrs -----------------------------
         77  Converter-UTF-8        pic X(32) value Z"utf-8".
         77  Converter-ARABIC       pic X(32) value Z"iso-8859-6".
         77  Converter-CYRILLIC     pic X(32) value Z"iso-8859-5".
         77  Converter-GREEK        pic X(32) value Z"iso-8859-7".
         77  CONVERTER-WIN-GREEK    pic X(32) value Z"windows-1253".
         77  CONVERTER-HEBREW       pic X(32) value Z"iso-8859-8".
         77  CONVERTER-WIN-HEBREW   pic X(32) value Z"windows-1255".
      *>
      *>-------------- Ported from ucol.h -----------------------------
      *>     *> Possible values for a comparison result *>/
      *>    typedef enum {
      *>      /*>*> String a == String b *>/
      *>      UCOL_EQUAL    = 0,
      *>      /*>*> String a > String b *>/
      *>      UCOL_GREATER    = 1,
      *>      /*>*> String a < String b *>/
      *>      UCOL_LESS    = -1
      *>    } UCollationResult ;
       01  UCollationResult                pic S9(9) Binary value 0.
         88  UCOL-EQUAL                        value 0.
         88  UCOL-GREATER                      value 1.
         88  UCOL-LESS                         value -1.
      *>
      *>-------------- Ported from utypes.h -----------------------------
      *>                                 UErrorCode *>status);
       01  UErrorCode                      pic S9(9) Binary value 0.
      *>     Start of information results (semantiCally successful)
         88  U-ERROR-INFO-START                       value -128.
      *>     A resource bundle lookup returned a fallback result
      *>     (not an error)
         88  U-USING-FALLBACK-ERROR                   value -128.
      *>     A resource bundle lookup returned a result from the root
      *>      locale (not an error)
         88  U-USING-DEFAULT-ERROR                    value -127.
      *>     A SafeClone operation required allocating memory
      *>     (informational only)
         88  U-SAFECLONE-ALLOCATED-ERROR              value -126.
      *>     This must always be the last warning value to indicate the
      *>      limit for UErrorCode warnings (last warning code +1)
         88  U-ERROR-INFO-LIMIT                       value  0.
      *>     No error. no warning.
         88  U-ZERO-ERROR                             value  0.
      *>     Start of codes indicating failure
         88  U-ILLEGAL-ARGUMENT-ERROR                 value  1.
         88  U-MISSING-RESOURCE-ERROR                 value  2.
         88  U-INVALID-FORMAT-ERROR                   value  3.
         88  U-FILE-ACCESS-ERROR                      value  4.
      *>     Indicates a bug in the library code
         88  U-INTERNAL-PROGRAM-ERROR                 value  5.
         88  U-MESSAGE-PARSE-ERROR                    value  6.
      *>     Memory allocation error
         88  U-MEMORY-ALLOCATION-ERROR                value  7.
         88  U-INDEX-OUTOFBOUNDS-ERROR                value  8.
      *>     Equivalent to Java ParseException
         88  U-PARSE-ERROR                            value  9.
      *>     In the Character conversion routines:
      *>     Invalid character or sequence was encountered
         88  U-INVALID-CHAR-FOUND                     value 10.
      *>     In the Character conversion routines:
      *>     More bytes are required to complete the conversion
      *>     successfully
         88  U-TRUNCATED-CHAR-FOUND                   value 11.
      *>     In codeset conversion: a sequence that does NOT belong in
      *>      the codepage has been encountered
         88  U-ILLEGAL-CHAR-FOUND                     value 12.
      *>     Conversion table file found. but corrupted
         88  U-INVALID-TABLE-FORMAT                   value 13.
      *>     Conversion table file not found
         88  U-INVALID-TABLE-FILE                     value 14.
      *>     A result would not fit in the supplied Buffer
         88  U-BUFFER-OVERFLOW-ERROR                  value 15.
      *>     Requested operation not supported in current context
         88  U-UNSUPPORTED-ERROR                      value 16.
      *>     an operation is requested over a resource
      *>     that does not support it
         88  U-RESOURCE-TYPE-MISMATCH                 value 17.
      *>     ISO-2022 illegal escape sequence
         88  U-ILLEGAL-ESCAPE-SEQUENCE                value 18.
      *>     ISO-2022 unsupported escape sequence
         88  U-UNSUPPORTED-ESCAPE-SEQUENCE            value 19.
      *>     No space available for in-Buffer expansion for Arabic
      *>     shaping
         88  U-NO-SPACE-AVAILABLE                     value 20.
      *>     This must always be the last value to indicate the limit for
      *>     UErrorCode (last error code +1)
         88  U-ERROR-LIMIT                            value 21.
      *>     U-SUCCESS and U-FAILURE as 88s instead of C macros
         88  U-SUCCESS                             values -128 thru 0.
         88  U-FAILURE                             values 1 thru 21.
      *>
      *>=================================================================
      *>
       Linkage section.
       01  Converter-Name-Link.
         03  Converter-Name-String            pic X(80).
      *>
      *>====================== Main Program  ============================
      *>
       PROCEDURE DIVISION using Converter-Name-Link.
       MAIN section.
      *>
      *>---------------------- Set Online debugging ---------------------
      *>
           Move    1 to Debug-Display-sw.
           Move    1 to Debug-Write-Sw.
           Perform Debug-Open-sec.
           Move ">>> Program: ICU-Coll - ver 2.0 - 20.01.02"
                                                   to Debug-Text.
           Perform Debug-Display-sec.
      *>
           Perform Input-Open-sec.
           Perform Output-Open-sec.
      *>
           Move    DLL-Coll-Name to DLL-Name.
           Perform Load-DLL-sec.
           Move    DLL-Handle      to DLL-Coll-Handle.
      *>
           Move    DLL-Conv-Name to DLL-Name.
           Perform Load-DLL-sec.
      *>
           Perform Open-Unicode-Collator-sec.
           Perform Open-Unicode-Converter-sec.
           IF U-SUCCESS
             Perform   Collation-Input-Read-sec
                       Varying Main-index  FROM 1 by 1
                       until               Input-Read-Flag = 0
             Move      1           to      Sort-Do-sw
             Perform   Collation-Main-Sort-sec
                       Varying Main-index  FROM 1 by 1
                       until   Main-index  = Input-Record-Number
                          or   Sort-Do-sw  = 0
             Perform   Collation-Output-Write-sec
                       Varying Main-index  FROM 1 by 1
                       until   Main-index  > Input-Record-Number
             Perform   Close-Unicode-Converter-sec
             Perform   Close-Unicode-Collator-sec
           End-if.
      *>
      *>----------------- Close and Free buffers ------------------------
      *>
           Perform Free-DLL-sec.
           Perform Free-DLL-Coll-sec.
      *>
           Perform Input-Close-sec.
           Perform Output-Close-sec.
           Perform Debug-Close-sec.
      *>
           Stop Run.
      *>
      *>================= ICU Collation Main Loop ===================
      *>
       Collation-Input-Read-sec  section.
       Collation-Input-Read.
           Perform   Input-Read-sec.
           IF    Input-Read-Flag = 1 then
             Perform   Convert-to-Unicode-sec
             Move      Unicode-Input-Buffer
                  to   Unicode-Buffer-Array (Main-Index)
             Compute   Input-Record-Number = Input-Record-Number + 1
           End-if.
       Collation-Input-Read-ex.
           Exit.
      *>
       Collation-Main-Sort-sec  section.
       Collation-Main-Sort.
           Move      ZERO    to      Sort-Do-sw.
           Compute   Sort-Count = Input-Record-Number - Main-Index.
           Perform   Collation-Sort-sec
                     Varying Sort-index  FROM 1 by 1
                     until   Sort-index  > Sort-Count.
       Collation-Main-Sort-ex.
           Exit.
      *>
       Collation-Sort-sec  section.
       Collation-Sort.
           Compute   Index-First  = Sort-Index.
           Compute   Index-Second = Sort-Index  + 1.
      *>
           Perform   Str-Unicode-Collator-sec.
           If UCOL-GREATER then
             Move   1             to      Sort-Do-sw
             Move   Unicode-Buffer-Array (Index-First)
               to   Unicode-Input-Buffer
             Move   Unicode-Buffer-Array (Index-Second)
               to   Unicode-Buffer-Array (Index-First)
             Move   Unicode-Input-Buffer
               to   Unicode-Buffer-Array (Index-Second)
           End-if.
       Collation-Sort-ex.
           Exit.
      *>
       Collation-Output-Write-sec  section.
       Collation-Output-Write.
           Move      Unicode-Buffer-Array (Main-Index)
                to   Unicode-Output-Buffer.
           Perform   Convert-from-Unicode-sec.
           Perform   Output-Write-sec.
       Collation-Output-Write-ex.
           Exit.
      *>
      *>================= Load/Free ICU Libraries (Call "LoadLibraryA") =
      *>
       Load-DLL-sec section.
       Load-DLL.
           Move    "Call :    LoadLibraryA" to Debug-Text.
           Move    DLL-Name                 to Debug-Value.
           Perform Debug-Display-sec.
           Call    "LoadLibraryA"  using by reference  DLL-Name
                                   Returning           DLL-Handle.
           IF DLL-Handle = ZEROS
              Move     "Couldn't load " to Debug-Text
              Move     DLL-Name         to Debug-Value
              Move     1                to Error-Display-sw
              Perform  Debug-Display-sec
              Move
            "Check that your PATH includes the directory with the DLLs."
                                        to Debug-Buffer
              Move     1                to Error-Display-sw
              Perform  Debug-Display-sec
              Stop Run.
        Load-DLL-ex.
           Exit.
      *>
       Free-DLL-sec section.
       Free-DLL.
           Move    "Call :    FreeLibrary" to Debug-Text.
           Move    DLL-Name                to Debug-Value
           Perform Debug-Display-sec
           Call    "FreeLibrary"        using DLL-Handle.
       Free-DLL-ex.
           Exit.
       Free-DLL-Coll-sec section.
       Free-DLL-Coll.
           Move    "Call :    FreeLibrary" to Debug-Text.
           Move    Z"ICUIN20.DLL"          to Debug-Value
           Perform Debug-Display-sec
           Call    "FreeLibrary"        using DLL-Coll-Handle.
       Free-DLL-Coll-ex.
           Exit.
      *>
      *>==================== Open API in DLL (Call "GetProcAddress") ====
      *>
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
      *>
       Get-API-Coll-Pointer-sec section.
       Get-API-Coll-Pointer.
           Move    "Call API Function :" to Debug-Text.
           Move    API-Name              to Debug-Value.
           Perform Debug-Display-sec.
           Call    "GetProcAddress" using by value DLL-Coll-Handle
                                       by reference API-Name
                                       Returning API-Pointer.
           IF API-Pointer = NULL
              Move     "The following API was not found in DLL:"
                                         to Debug-Text
              Move     API-Name          to Debug-Value
              Move     1                 to Error-Display-sw
              Perform  Debug-Display-sec
              Stop Run.
       Get-API-Coll-Pointer-ex.
           Exit.
      *>
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
      *>
      *>================== Collation APIs  ==============================
      *>
       Open-Unicode-Collator-sec section.
       Open-Unicode-Collator.
      *>  --------------------- UCOL.H ---------------------------
      *>   U_CAPI UCollator*> U_EXPORT2  ucol_open(
      *>                         const char *> loc,
      *>                         UErrorCode *> status )
      *>   Opens a UCollator for comparing Strings
      *>   The UCollator Pointer is used in all the calls to the
      *>     Collation service.  When finished, the collator must be
      *>     disposed of by calling ucol_close.
      *>   Parameters:
      *>     loc:    the locale containing the required collation rules.
      *>             if this parm is NULL, UCA rules will be used.
      *>     status: pointer to an UErrorCode to receive any errors
      *>
      *>   Returns: pointer to a UCollator, or 0 if an error occurred
           STRING "ucol_open" ICU-VERSION-SUFFIX
               delimited by size into API-Name.
           Perform Get-API-Coll-Pointer-sec.
           Call API-Pointer using by reference Collator-Loc
                                  by reference UErrorCode
                                  Returning    Collator-Pointer.
           Perform Check-Call-to-API-sec.
       Open-Unicode-Collator-ex.
           Exit.
      *>
       Str-Unicode-Collator-sec section.
       Str-Unicode-Collator.
      *>  --------------------- UCOL.H ---------------------------
      *>   U_CAPI UCollationResult U_EXPORT2  ucol_strcoll(
      *>                         const UCollator *> coll,
      *>                         const UChar *> source,
      *>                         int32_t sourceLength,
      *>                         const UChar *> target,
      *>                         int32_t targetLength )
      *>   Compares two strings
      *>   The strings will be compared using the options specified.

      *>   Parameters:
      *>     coll:          UCollator containing the comparison rules
      *>     source:        source string.
      *>     sourceLength:  length of source, or -1 if null-terminated
      *>     target:        target string.
      *>     targetLength:  length of target, or -1 if null-terminated
      *>   Returns  result of comparing the Strings;
      *>     one of UCOL_EQUAL, UCOL_GREATER, UCOL_LESS
           STRING "ucol_strcoll" ICU-VERSION-SUFFIX
               delimited by size into API-Name.
           Perform Get-API-Coll-Pointer-sec.
           Call API-Pointer using by value     Collator-Pointer
                                  by reference
                                     Unicode-Buffer-Array (Index-First)
                                  by value     U-Text-Length
                                  by reference
                                     Unicode-Buffer-Array (Index-Second)
                                  by value     U-Text-Length
                                  by reference UErrorCode
                                  Returning    UCollationResult.
           Perform Check-Call-to-API-sec.
      *>
      *>-------------- Ported from ucol.h -----------------------------
      *>     *> Possible values for a comparison result *>/
      *>    typedef enum {
      *>      /*>*> String a == String b *>/
      *>      UCOL_EQUAL    = 0,
      *>      /*>*> String a > String b *>/
      *>      UCOL_GREATER    = 1,
      *>      /*>*> String a < String b *>/
      *>      UCOL_LESS    = -1
      *>    } UCollationResult ;
           Move "ucol_strcoll - UCollationResult:"
                                       to      Debug-Text.
           IF UCOL-EQUAL
             Move      "UCOL-EQUAL"    to      Debug-Value.
           IF UCOL-GREATER
             Move      "UCOL-GREATER"  to      Debug-Value.
           IF UCOL-LESS
             Move      "UCOL-LESS"     to      Debug-Value.
           Perform  Debug-Display-sec.
       Str-Unicode-Collator-ex.
           Exit.
      *>
       Close-Unicode-Collator-sec section.
       Close-Unicode-Collator.
      *>  --------------------- UCNV.H ---------------------------
      *>   U_CAPI void  U_EXPORT2  ucol_close( UCollator  *> coll )
      *>   Deletes the collator and releases associated resources
           STRING "ucol_close" ICU-VERSION-SUFFIX
               delimited by size into API-Name.
           Perform Get-API-Coll-Pointer-sec.
           Call API-Pointer using by value Collator-Pointer.
           Perform Check-Call-to-API-sec.
       Close-Unicode-Collator-ex.
           Exit.
      *>
      *>==================    Converters APIs   =========================
      *>
       Open-Unicode-Converter-sec section.
       Open-Unicode-Converter.
      *>  --------------------- UCNV.H ---------------------------
      *>   U_CAPI UConverter *>U_EXPORT2  ucnv_open(
      *>                         const char *> ConverterName,
      *>                         UErrorCode *> err )
      *>   Creates a UConverter object with the name
      *>         specified as a C String
           STRING "ucnv_open" ICU-VERSION-SUFFIX
               delimited by size into API-Name.
           Perform Get-API-Pointer-sec.
      *>    --- convert from input file to Unicode ---------------
           Call API-Pointer using by reference Converter-WIN-HEBREW
                                  by reference UErrorCode
                                  Returning    Converter-toU-Pointer.
           Perform Check-Call-to-API-sec.
      *>    --- convert from Unicode to output file  ---------------
           Call API-Pointer using by reference Converter-HEBREW
                                  by reference UErrorCode
                                  Returning    Converter-fromU-Pointer.
           Perform Check-Call-to-API-sec.
       Open-Unicode-Converter-ex.
           Exit.
      *>
       Convert-to-Unicode-sec section.
       Convert-to-Unicode.
      *>  --------------------- UCNV.H ---------------------------
      *>   U_CAPI int32_t U_EXPORT2  ucnv_toUChars(
      *>                         UConverter *> cnv,
      *>                         UChar *> dest,
      *>                         int32_t destCapacity,
      *>                         const char *> src,
      *>                         int32_t srcLength,
      *>                         UErrorCode *> pErrorCode )
      *>   Convert the codepage String into a Unicode String using
      *>                            an existing UConverter.
      *>   Returns: length of the output string, not counting the
      *>     terminating NUL; if the length is greater than destCapacity,
      *>     then the string will not fit and a buffer of the indicated
      *>     length would need to be passed in.
      *>
           STRING "ucnv_toUChars" ICU-VERSION-SUFFIX
               delimited by size into API-Name.
           Perform Get-API-Pointer-sec.
           Call API-Pointer using by value     Converter-toU-Pointer
                                  by reference Unicode-Input-Buffer
                                  by value     U-destCapacity
                                  by reference Input-Buffer
                                  by value     Text-Length
                                  by reference UErrorCode
                                  Returning    U-Text-Length.
           Perform Check-Call-to-API-sec.
       Convert-to-Unicode-ex.
           Exit.
      *>
      *>====================== Convert from Unicode APIs  ===============
      *>
       Convert-from-Unicode-sec section.
       Convert-from-Unicode.
      *>  --------------------- UCNV.H ---------------------------
      *>   U_CAPI int32_t U_EXPORT2  ucnv_fromUChars(
      *>                         UConverter *> cnv,
      *>                         char *> dest,
      *>                         int32_t destCapacity,
      *>                         const UChar *> src,
      *>                         int32_t srcLength,
      *>                         UErrorCode *> pErrorCode )
      *>   Converts a Unicode string into a codepage string using
      *>         an existing UConverter
      *>   Returns: length of the output String, not counting
      *>     the terminating NUL; if the length is greater than
      *>     destCapacity, then the string will not fit and
      *>     a buffer of the indicated length would need to be passed in.
           IF U-SUCCESS
              Move SPACES to Output-Buffer
              STRING "ucnv_fromUChars" ICU-VERSION-SUFFIX
                  delimited by size into API-Name
              Perform Get-API-Pointer-sec
              Call API-Pointer
                   using by value      Converter-fromU-Pointer
                         by reference  Output-Buffer
                         by value      destCapacity
                         by reference  Unicode-Output-Buffer
                         by value      U-Text-Length
                         by reference  UErrorCode
                         Returning     Text-Length
              Perform Check-Call-to-API-sec.
       Convert-from-Unicode-ex.
           Exit.
      *>
       Close-Unicode-Converter-sec section.
       Close-Unicode-Converter.
      *>  --------------------- UCNV.H ---------------------------
      *>   U_CAPI void  U_EXPORT2  ucnv_close( UConverter *> converter )
      *>   Deletes Unicode converter and releases associated resources
           STRING "ucnv_close" ICU-VERSION-SUFFIX
               delimited by size into API-Name
           Perform Get-API-Pointer-sec.
           Call API-Pointer using by value Converter-Pointer.
           Perform Check-Call-to-API-sec.
       Close-Unicode-Converter-ex.
           Exit.
      *>
      *>========================  Input File  =====================
      *>
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
      *>
      *>---------------------   Read the record  -----------------------
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
           End-If.
       Input-Read-ex.
           Exit.
      *>
      *>-----------------------  Close the file  ------------------------
       Input-Close-sec section.
       Input-Close.
           Close Input-File.
       Input-Close-ex.
           Exit.
      *>
      *>========================  Output File  =====================
      *>
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
      *>
      *>---------------------   Write the record  -----------------------
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
      *>
      *>-----------------------  Close the file  ------------------------
       Output-Close-sec section.
       Output-Close.
           Close       Output-File.
       Output-Close-ex.
           Exit.
      *>
      *>=================      Debuging  Sesions   ======================
      *>
      *>--------------------  Open Debug Log File  ----------------------
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
      *>--------------------  Close Debug Log File  ---------------------
       Debug-Close-sec section.
       Debug-Close.
           Close Debug-File.
       Debug-Close-ex.
           Exit.
      *>-----------------  Write / Display Debug Information  -----------
       Debug-Display-sec section.
       Debug-Display.
      *>    Truncate output at first null character
           INSPECT Debug-Value replacing characters by Spaces
               after initial X"00" first X"00" by Space.
           IF Debug-Display-sw = 1 Or Error-Display-sw = 1
             Move 0 to Error-Display-sw
             Display Debug-Buffer.
           IF  Debug-Write-sw = 1 Then
             Write Debug-Record from Debug-Buffer
           End-If.
           Move " " to Debug-Value.
       Debug-Display-ex.
           Exit.
