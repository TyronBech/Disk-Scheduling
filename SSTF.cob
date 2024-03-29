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
       01 WS-PROCESSES OCCURS 0 TO 100 TIMES DEPENDING ON WS-NO-PROC.
         02 WS-PROC PIC 9(3) VALUE ZEROES.
       01 WS-SEQUENCE OCCURS 0 TO 100 TIMES DEPENDING ON K.
         02 WS-SEQ PIC 9(3) VALUE ZEROES.
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
      *    GETTING THE NUMBER OF CYLINDERS
           DISPLAY "ENTER NUMBER OF CYLINDERS: " WITH NO ADVANCING.
           ACCEPT WS-CYLINDER.
           MOVE WS-CYLINDER TO WS-CURRENT.
      *    GETTING THE NUMBER OF PROCESSES
           DISPLAY "ENTER NO. OF PROCESS: " WITH NO ADVANCING.
           ACCEPT WS-NO-PROC.
      *    ADDING 1 TO WS-NO-PROC TO INCLUDE THE STARTING POINT
           ADD 1 TO WS-NO-PROC.
      *    GETTING THE STARTING POINT
           PERFORM UNTIL WS-CURRENT < WS-CYLINDER
             DISPLAY "ENTER THE STARTING POINT: " WITH NO ADVANCING
             ACCEPT WS-CURRENT
             IF WS-CURRENT > WS-CYLINDER
               DISPLAY "YOU EXCEED THE SIZE OF CYLINDER"
             END-IF
           END-PERFORM.
      *    ADDING THE STARTING POINT TO THE TABLE
           MOVE WS-CURRENT TO WS-PROCESSES(1)
      *    LOOP FOR GETTING INPUTS
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > WS-NO-PROC
             DISPLAY "ENTER A PROCESS: " WITH NO ADVANCING
             ACCEPT WS-PR
             IF WS-PR > WS-CYLINDER THEN
               DISPLAY "YOU EXCEED THE SIZE OF CYLINDER"
               SUBTRACT 1 FROM I
             ELSE
      *        MOVING THE INPUT TO THE TABLE
               MOVE WS-PR TO WS-PROCESSES(I)
             END-IF
           END-PERFORM.
      *    SORTING THE TABLE ASCENDINGLY
           PERFORM BUBBLE-SORT.
      *    FINDING THE CURRENT HEAD ON THE SORTED TABLE
           PERFORM FIND-CURRENT.
      *    SETTING THE K TO 1 TO PUT THE CURRENT HEAD ON SEQUESNCE TABLE
           ADD 1 TO K.
           MOVE WS-CURRENT TO WS-SEQUENCE(K).
      *    ADDING 1 AGAIN TO K TO PUT THE NEXT ELEMENT NOT IN INDEX 1
           ADD 1 TO K.
           PERFORM SORT-SEQUENCE WS-NO-PROC TIMES.
           PERFORM SEQ-COMPUTE.
           STOP RUN.
      * SORTING FUNCTION FOR ARRAY OF INPUTS
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
      * SORTING THE ARRAY BASED ON THE LOGIC OF SSTF AND PUT ON SEPARATE
      * TABLE
       SORT-SEQUENCE.
           IF WS-CURRENT-IDX > 1 OR WS-CURRENT-IDX <
             WS-NO-PROC THEN
      *      DIFFERENT SET OF MOVE IF THE K IS LESS OR EQUAL TO 2
             IF K <= 2 THEN
               MOVE WS-PROCESSES(WS-CURRENT-IDX - 1) TO WS-MIN
               MOVE WS-PROCESSES(WS-CURRENT-IDX + 1) TO WS-MAX
               MOVE WS-PROCESSES(WS-CURRENT-IDX) TO WS-CUR
             END-IF
      *      COMPUTING FOR THE DIFFERENCES THEN COMPARE IT
             COMPUTE I = WS-CUR - WS-MIN
             COMPUTE J = WS-MAX - WS-CUR
      *    IDENTIFYING IF THE NEXT HAD MOVEMENT IS HIGHER OR LOWER
      *    IF THE SHORTEST PATH IS ON THE LEFT THE WE CALL THE MOVE-MIN
      *    ELSE MOVE-MAX ALSO THE SAME IF ONE OF THEM IS EQUAL 0
             IF WS-MAX = 0 THEN
               PERFORM MOVE-MIN
             ELSE IF WS-MIN = 0 THEN
               PERFORM MOVE-MAX
             ELSE IF I <= J THEN
               PERFORM MOVE-MIN
             ELSE IF WS-MIN = 0 OR J < I THEN
               PERFORM MOVE-MAX
             END-IF
           END-IF.
      *    ADDING 1 TO K TO MOVE THE INDEX FOR NEXT INPUTS
           ADD 1 TO K.
           EXIT.
      * MOVING THE CURRENT TO MIN VARIABLE THEN STORE IT TO THE TABLE
       MOVE-MIN.
           MOVE WS-MIN TO WS-SEQUENCE(K).
           MOVE WS-MIN TO WS-CURRENT.
           PERFORM FIND-CURRENT.
           MOVE WS-PROCESSES(WS-CURRENT-IDX) TO WS-CUR.
           PERFORM FIND-CURRENT.
           IF WS-CURRENT-IDX > 1 THEN
             MOVE WS-PROCESSES(WS-CURRENT-IDX - 1) TO WS-MIN
           ELSE
             MOVE 0 TO WS-MIN
           END-IF.
           EXIT.
      * MOVING THE CURRENT TO MAX VARIABLE THEN STORE IT TO THE TABLE
       MOVE-MAX.
           MOVE WS-MAX TO WS-SEQUENCE(K).
           MOVE WS-MAX TO WS-CURRENT.
           PERFORM FIND-CURRENT.
           MOVE WS-PROCESSES(WS-CURRENT-IDX) TO WS-CUR.
           PERFORM FIND-CURRENT.
           IF WS-CURRENT-IDX < WS-NO-PROC THEN
             MOVE WS-PROCESSES(WS-CURRENT-IDX + 1) TO WS-MAX
           ELSE
             MOVE 0 TO WS-MAX
           END-IF.
           EXIT.
      * LOOP TO COMPUTE THE SEQUENCE ARRAY ASCENDINGLY
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
      * FUNCTION TO FIND THE CURRENT POSITION OF THE CURRENT HEAD
       FIND-CURRENT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= WS-NO-PROC
             IF WS-PROCESSES(I) IS EQUAL TO WS-CURRENT
               MOVE I TO WS-CURRENT-IDX
             END-IF
           END-PERFORM.
           EXIT.
       END PROGRAM SSTF-DISK-SCHEDULING.
