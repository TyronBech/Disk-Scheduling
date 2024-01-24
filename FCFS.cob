      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISK-SCHEDULING.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-SIZE PIC 99 VALUE ZEROES.
       01 WS-PROC PIC 99 VALUE ZEROES.
       01 WS-NUM-PROC PIC 9(2) VALUE ZEROES.
       01 WS-HEAD PIC 99 VALUE ZEROES.
       01 TEMP PIC 9(2).
       01 WS-P1S PIC 99 VALUE ZEROES.
       01 WS-P2S PIC 99 VALUE ZEROES.
       01 WS-SUM PIC 999 VALUE ZEROES.
       01 WS-RES PIC 999 VALUE ZEROES.
       01 WS-PROCESSES OCCURS 30 TIMES INDEXED BY WS-IDX.
           02 WS-PROCESS PIC 9(2) VALUE ZEROES.
       01 WS-P-SUM OCCURS 30 TIMES INDEXED BY WS-ID-X.
           02 WS-S PIC 9(2) VALUE ZEROES.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "FIRST COME FIRST SERVE".
            DISPLAY "ENTER DISK SIZE: " WITH NO ADVANCING.
            ACCEPT WS-SIZE.
            DISPLAY "ENTER NUMBER OF PROCESS: " WITH NO ADVANCING.
            ACCEPT WS-NUM-PROC.
            DISPLAY "ENTER STARTING POINT: " WITH NO ADVANCING.
            ACCEPT WS-HEAD.
            MOVE WS-HEAD TO WS-PROCESSES(1).
            PERFORM VARYING TEMP FROM 2 BY 1 UNTIL TEMP > WS-NUM-PROC
            + 1
               DISPLAY "ENTER A PROCESS: " WITH NO ADVANCING
               ACCEPT WS-PROC
               SET WS-IDX TO TEMP
               MOVE WS-PROC TO WS-PROCESSES(WS-IDX)
            END-PERFORM.
            PERFORM COMPUTE-P.
                DISPLAY "SUM: " WS-SUM.
            STOP RUN.
       COMPUTE-P.
           PERFORM VARYING TEMP FROM 1 BY 1 UNTIL TEMP >
           WS-NUM-PROC
           COMPUTE WS-IDX = TEMP
               SET WS-P1S TO WS-PROCESSES(WS-IDX + 1)
               SET WS-P2S TO WS-PROCESSES(WS-IDX)
           COMPUTE WS-RES = WS-P1S - WS-P2S
           MOVE TEMP TO WS-ID-X
           COMPUTE WS-HEAD = WS-RES
           DISPLAY "HEAD MOVEMENT: " WS-RES
           COMPUTE WS-SUM = WS-SUM + WS-HEAD
           END-PERFORM.
           EXIT.
       END PROGRAM DISK-SCHEDULING.
