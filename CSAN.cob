      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. C-SCAN-DISK-ALGORITHM.
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
       01 WS-ALPHA PIC 9(3) VALUE ZEROES.
       01 WS-CYLINDER PIC 9(3) VALUE ZEROES.
       01 WS-PROCESSES OCCURS 0 TO 100 DEPENDING ON WS-NO-PROC.
         02 WS-PROC PIC 9(3) VALUE ZEROES.
       01 WS-PR PIC 9(3) VALUE ZEROES.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "C-SCAN DISK ALGORITHM".
           DISPLAY "ENTER NUMBER OF CYLINDERS: " WITH NO ADVANCING.
           ACCEPT WS-CYLINDER.
           SUBTRACT 1 FROM WS-CYLINDER.
           DISPLAY "ENTER NO. OF PROCESS: " WITH NO ADVANCING.
           ACCEPT WS-NO-PROC.
           ADD 3 TO WS-NO-PROC.
           MOVE WS-CYLINDER TO WS-PROCESSES(WS-NO-PROC).
           DISPLAY "ENTER THE STARTING POINT: " WITH NO ADVANCING.
           ACCEPT WS-START.
           DISPLAY "ENTER PREVIOUS POSITION: " WITH NO ADVANCING.
           ACCEPT WS-PREV.
           MOVE ZERO TO WS-PROCESSES(1).
           MOVE WS-START TO WS-PROCESSES(2).
           DISPLAY "ENTER ALPHA: " WITH NO ADVANCING.
           ACCEPT WS-ALPHA.
           PERFORM VARYING I FROM 3 BY 1 UNTIL I > WS-NO-PROC - 1
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
           PERFORM VARYING I FROM WS-START-IDX BY -1 UNTIL I < 2
             MOVE WS-PROCESSES(I) TO WS-P1S
             MOVE WS-PROCESSES(I - 1) TO WS-P2S
             COMPUTE WS-HEAD-M = WS-P1S - WS-P2S
             DISPLAY "HEAD MOVEMENT: " WS-HEAD-M
             COMPUTE WS-THM = WS-THM + WS-HEAD-M
           END-PERFORM.
           IF WS-START-IDX >= WS-NO-PROC THEN
             EXIT
           END-IF.
           COMPUTE WS-THM = WS-THM + WS-ALPHA.
           PERFORM VARYING I FROM WS-NO-PROC BY -1 UNTIL I <=
           WS-START-IDX + 1
               MOVE WS-PROCESSES(I) TO WS-P1S
               MOVE WS-PROCESSES(I - 1) TO WS-P2S
             COMPUTE WS-HEAD-M = WS-P1S - WS-P2S
             DISPLAY "HEAD MOVEMENT: " WS-HEAD-M
             COMPUTE WS-THM = WS-THM + WS-HEAD-M
           END-PERFORM.
           DISPLAY "THM: " WS-THM.
           EXIT.
       LOHI.
           PERFORM VARYING I FROM WS-START-IDX BY 1 UNTIL I >=
           WS-NO-PROC
             MOVE WS-PROCESSES(I + 1) TO WS-P1S
             MOVE WS-PROCESSES(I) TO WS-P2S
             COMPUTE WS-HEAD-M = WS-P1S - WS-P2S
             DISPLAY "HEAD MOVEMENT: " WS-HEAD-M
             COMPUTE WS-THM = WS-THM + WS-HEAD-M
           END-PERFORM.
           IF WS-START-IDX <= 1 THEN
             EXIT
           END-IF.
           COMPUTE WS-THM = WS-THM + WS-ALPHA.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >=
           WS-START-IDX - 1
               MOVE WS-PROCESSES(I  + 1) TO WS-P1S
               MOVE WS-PROCESSES(I) TO WS-P2S
             COMPUTE WS-HEAD-M = WS-P1S - WS-P2S
             DISPLAY "HEAD MOVEMENT: " WS-HEAD-M
             COMPUTE WS-THM = WS-THM + WS-HEAD-M
           END-PERFORM.
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
       END PROGRAM C-SCAN-DISK-ALGORITHM.
