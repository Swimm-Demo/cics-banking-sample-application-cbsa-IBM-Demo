      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2022                                      *
      *                                                                *
      ******************************************************************
       CBL CICS('SP,EDF')

       IDENTIFICATION DIVISION.
       PROGRAM-ID. GETSCODE.
       AUTHOR. James O'Grady.


       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER.   IBM-370 WITH DEBUGGING MODE.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.

       INPUT-OUTPUT SECTION.


       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.
      * Copyright statement as a literal to go into the load module
       77 FILLER PIC X(24) VALUE 'Copyright IBM Corp. 2022'. 

       COPY SORTCODE REPLACING ==SORTCODE== BY ==LITERAL-SORTCODE==.


       LINKAGE SECTION.
       01 DFHCOMMAREA.
           COPY GETSCODE.


       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       A010.
           MOVE LITERAL-SORTCODE
           TO SORTCODE OF DFHCOMMAREA.


           EXEC CICS RETURN
           END-EXEC.

           GOBACK.

