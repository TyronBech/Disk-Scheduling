      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SSTF-DISK-SCHEDULING.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-CYLINDER PIC 9(3).
       01  WS-NO-PROC PIC 9(3).
       01  WS-START PIC 9(3).
       01 WS-PROCESSES OCCURS 0 TO 100 TIMES DEPENDING ON WS-NO-PROC.
         02 WS-PROC PIC 9(3) VALUE ZEROES.
       01 WS-SEQUENCE OCCURS 0 TO 100 TIMES DEPENDING ON K.
         02 WS-SEQ PIC 9(3) VALUE ZEROES.
       01 WS-START-IDX PIC 9(3).
       01 WS-CURRENT-IDX PIC 9(3).
       01 WS-CURRENT PIC 9(3).
       01 WS-HEAD-M PIC 9(3).
       01 TEMP PIC 9(3).
       01 I PIC 9(3).
       01 J PIC 9(3).
       01 K PIC 9(3).
       01 WS-P1S PIC 9(3).
       01 WS-P2S PIC 9(3).
       01 WS-PR PIC 9(3).
       01 WS-MIN PIC 9(3).
       01 WS-MAX PIC 9(3).
       01 WS-CUR PIC 9(3).
       01 WS-THM PIC 9(3).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "SHORTEST SEEK TIME FIRST DISK ALGORITHM".
           DISPLAY "ENTER NUMBER OF CYLINDERS: " WITH NO ADVANCING.
           ACCEPT WS-CYLINDER.
           DISPLAY "ENTER NO. OF PROCESS: " WITH NO ADVANCING.
           ACCEPT WS-NO-PROC.
           ADD 1 TO WS-NO-PROC.
           DISPLAY "ENTER THE STARTING POINT: " WITH NO ADVANCING.
           ACCEPT WS-START.
           MOVE WS-START TO WS-PROCESSES(1)
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > WS-NO-PROC
             DISPLAY "ENTER A PROCESS: " WITH NO ADVANCING
             ACCEPT WS-PR
             MOVE WS-PR TO WS-PROCESSES(I)
           END-PERFORM.
           PERFORM BUBBLE-SORT.
           PERFORM FIND-START.
           ADD 1 TO K.
           MOVE WS-START TO WS-SEQUENCE(K).
           ADD 1 TO K.
           PERFORM SORT-SEQUENCE WS-NO-PROC TIMES.
      *    PERFORM DISP.
           PERFORM SEQ-COMPUTE.
           STOP RUN.
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
       SORT-SEQUENCE.
           IF WS-START-IDX > 1 OR WS-START-IDX <
             WS-NO-PROC THEN
             IF K <= 2 THEN
               MOVE WS-PROCESSES(WS-START-IDX - 1) TO WS-MIN
               MOVE WS-PROCESSES(WS-START-IDX + 1) TO WS-MAX
               MOVE WS-PROCESSES(WS-START-IDX) TO WS-CUR
               DISPLAY "K2 APPEAR"
             END-IF
             COMPUTE I = WS-CUR - WS-MIN
             COMPUTE J = WS-MAX - WS-CUR
             DISPLAY "I: " I
             DISPLAY "J: " J
             DISPLAY "WS-CUR: " WS-CUR
             DISPLAY "WS-MIN: " WS-MIN
             DISPLAY "WS-MAX: " WS-MAX
             IF WS-MAX = 0 THEN
               DISPLAY "MIN WINS"
               MOVE WS-MIN TO WS-SEQUENCE(K)
               MOVE WS-MIN TO WS-CURRENT
               MOVE WS-MIN TO WS-START
               PERFORM FIND-CURRENT
               MOVE WS-PROCESSES(WS-CURRENT-IDX) TO WS-CUR
               PERFORM FIND-START
               IF WS-CURRENT-IDX > 1 THEN
                 MOVE WS-PROCESSES(WS-START-IDX - 1) TO WS-MIN
               ELSE
                 MOVE 0 TO WS-MIN
               END-IF
             ELSE IF WS-MIN = 0 THEN
               DISPLAY "MAX WINS"
               MOVE WS-MAX TO WS-SEQUENCE(K)
               MOVE WS-MAX TO WS-CURRENT
               MOVE WS-MAX TO WS-START
               PERFORM FIND-CURRENT
               MOVE WS-PROCESSES(WS-CURRENT-IDX) TO WS-CUR
               PERFORM FIND-START
               IF WS-CURRENT-IDX < WS-NO-PROC THEN
                 MOVE WS-PROCESSES(WS-START-IDX + 1) TO WS-MAX
               ELSE
                 MOVE 0 TO WS-MAX
               END-IF
             ELSE IF I <= J THEN
               DISPLAY "MIN WINS"
               MOVE WS-MIN TO WS-SEQUENCE(K)
               MOVE WS-MIN TO WS-CURRENT
               MOVE WS-MIN TO WS-START
               PERFORM FIND-CURRENT
               MOVE WS-PROCESSES(WS-CURRENT-IDX) TO WS-CUR
               PERFORM FIND-START
               IF WS-CURRENT-IDX > 1 THEN
                 MOVE WS-PROCESSES(WS-START-IDX - 1) TO WS-MIN
               ELSE
                 MOVE 0 TO WS-MIN
               END-IF
             ELSE IF J < I THEN
               DISPLAY "MAX WINS"
               MOVE WS-MAX TO WS-SEQUENCE(K)
               MOVE WS-MAX TO WS-CURRENT
               MOVE WS-MAX TO WS-START
               PERFORM FIND-CURRENT
               MOVE WS-PROCESSES(WS-CURRENT-IDX) TO WS-CUR
               PERFORM FIND-START
               IF WS-CURRENT-IDX < WS-NO-PROC THEN
                 MOVE WS-PROCESSES(WS-START-IDX + 1) TO WS-MAX
               ELSE
                 MOVE 0 TO WS-MAX
               END-IF
             END-IF
           END-IF.
           ADD 1 TO K.
           EXIT.
       DISP.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > K
             DISPLAY "ARR AT: " I " IS " WS-SEQUENCE(I)
           END-PERFORM.
           EXIT.
       SEQ-COMPUTE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-NO-PROC - 1
             MOVE WS-SEQUENCE(I) TO WS-P1S
             MOVE WS-SEQUENCE(I + 1) TO WS-P2S
             IF WS-P1S >= WS-P2S THEN
               COMPUTE WS-HEAD-M = WS-P1S - WS-P2S
             ELSE
               COMPUTE WS-HEAD-M = WS-P2S - WS-P1S
             END-IF
             DISPLAY "HEAD MOVEMENT: " WS-HEAD-M
             COMPUTE WS-THM = WS-THM + WS-HEAD-M
           END-PERFORM.
           DISPLAY "THM: " WS-THM.
           EXIT.
       FIND-START.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= WS-NO-PROC
             IF WS-PROCESSES(I) IS EQUAL TO WS-START
               MOVE I TO WS-START-IDX
             END-IF
           END-PERFORM.
           EXIT.
       FIND-CURRENT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= WS-NO-PROC
             IF WS-PROCESSES(I) IS EQUAL TO WS-CURRENT
               MOVE I TO WS-CURRENT-IDX
             END-IF
           END-PERFORM.
           EXIT.
       END PROGRAM SSTF-DISK-SCHEDULING.
