      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOOK-DISK-ALGORITHM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NO-PROC PIC 9(2) VALUE ZEROES.
       01 WS-START PIC 9(3) VALUE ZEROES.
       01 WS-PREV PIC 9(3) VALUE ZEROES.
       01 I PIC 9(3) VALUE ZEROES.
       01 J PIC 9(3) VALUE ZEROES.
       01 TEMP PIC 9(3) VALUE ZEROES.
       01 WS-START-IDX PIC 9(3) VALUE ZEROES.
       01 WS-THM PIC 9(4) VALUE ZEROES.
       01 WS-HEAD-M PIC 9(4) VALUE ZEROES.
       01 WS-P1S PIC 9(3) VALUE ZEROES.
       01 WS-P2S PIC 9(3) VALUE ZEROES.
       01 WS-PROCESSES OCCURS 0 TO 100 DEPENDING ON WS-NO-PROC.
         02 WS-PROC PIC 9(3) VALUE ZEROES.
       01 WS-PR PIC 9(3) VALUE ZEROES.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "LOOK DISK ALGORITHM".
           DISPLAY "ENTER NO. OF PROCESS: " WITH NO ADVANCING.
           ACCEPT WS-NO-PROC.
           ADD 1 TO WS-NO-PROC.
           DISPLAY "ENTER THE STARTING POINT: " WITH NO ADVANCING.
           ACCEPT WS-START.
           DISPLAY "ENTER PREVIOUS POSITION: " WITH NO ADVANCING.
           ACCEPT WS-PREV.
           MOVE WS-START TO WS-PROCESSES(1).
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > WS-NO-PROC
             DISPLAY "ENTER A PROCESS: " WITH NO ADVANCING
             ACCEPT WS-PR
             MOVE WS-PR TO WS-PROCESSES(I)
           END-PERFORM.
           PERFORM BUBBLE-SORT.
           PERFORM FIND-START.
           IF WS-PREV > WS-START THEN
             PERFORM HILO
           ELSE
             PERFORM LOHI
           END-IF.

           STOP RUN.
       HILO.
           PERFORM VARYING TEMP FROM WS-START-IDX BY -1 UNTIL TEMP < 2
           MOVE WS-PROCESSES(TEMP) TO WS-P1S
           MOVE WS-PROCESSES(TEMP - 1) TO WS-P2S
           COMPUTE WS-HEAD-M = WS-P1S - WS-P2S
           DISPLAY "HEAD MOVEMENT: " WS-HEAD-M
           COMPUTE WS-THM = WS-THM + WS-HEAD-M
           END-PERFORM.
           MOVE WS-PROCESSES(WS-NO-PROC) TO WS-P1S.
           COMPUTE WS-HEAD-M = WS-P1S - WS-P2S.
           DISPLAY "HEAD MOVEMENT: " WS-HEAD-M.
           COMPUTE WS-THM = WS-THM + WS-HEAD-M.
           DISPLAY "THM: " WS-THM.
           EXIT.
       LOHI.
           MOVE WS-PROCESSES(WS-START-IDX) TO WS-P1S.
           MOVE WS-PROCESSES(WS-NO-PROC) TO WS-P2S.
           COMPUTE WS-HEAD-M = WS-P2S - WS-P1S.
           DISPLAY "HEAD MOVEMENT: " WS-HEAD-M.
           COMPUTE WS-THM = WS-THM + WS-HEAD-M.
           MOVE WS-PROCESSES(1) TO WS-P1S.
           COMPUTE WS-HEAD-M = WS-P2S - WS-P1S.
           DISPLAY "HEAD MOVEMENT: " WS-HEAD-M.
           COMPUTE WS-THM = WS-THM + WS-HEAD-M.
           DISPLAY "THM: " WS-THM.
           EXIT.
       BUBBLE-SORT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= WS-NO-PROC
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > (WS-NO-PROC - I)
               IF WS-PROCESSES(J) > WS-PROCESSES(j + 1) THEN
                 MOVE WS-PROCESSES(J) TO TEMP
                 MOVE WS-PROCESSES(J + 1) TO WS-PROCESSES(J)
                 MOVE TEMP TO WS-PROCESSES(J + 1)
               END-IF
             END-PERFORM
           END-PERFORM.
           EXIT.
       FIND-START.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= WS-NO-PROC
             IF WS-PROCESSES(I) IS EQUAL TO WS-START
               MOVE I TO WS-START-IDX
             END-IF
           END-PERFORM.
           EXIT.
       END PROGRAM LOOK-DISK-ALGORITHM.
