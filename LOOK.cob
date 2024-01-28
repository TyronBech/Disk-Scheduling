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
      *    INPUT THE NUMBER OF PROCESSES
           DISPLAY "ENTER NO. OF PROCESS: " WITH NO ADVANCING.
           ACCEPT WS-NO-PROC.
      *    ADD 1 TO THE NUMBER OF PROCESSES
           ADD 1 TO WS-NO-PROC.
      *    INPUT START
           DISPLAY "ENTER THE STARTING POINT: " WITH NO ADVANCING.
           ACCEPT WS-START.
      *    INPUT PREVIOUS
           DISPLAY "ENTER PREVIOUS POSITION: " WITH NO ADVANCING.
           ACCEPT WS-PREV.
           MOVE WS-START TO WS-PROCESSES(1).
      *    LOOP TO GET THE INOUTS
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > WS-NO-PROC
             DISPLAY "ENTER A PROCESS: " WITH NO ADVANCING
      *      ACCEPT INPUT
             ACCEPT WS-PR
      *      MOVE THE INPUT TO THE TABLE BASED ON CERTAIN INDEX
             MOVE WS-PR TO WS-PROCESSES(I)
           END-PERFORM.
      *    BUBBLE SORT
           PERFORM BUBBLE-SORT.
      *    FIND THE STARTING OF THE PROCESS
           PERFORM FIND-START.
      *    CONDITION FOR DIRECTION
      *    IF PREVIOUS HEAD GREATER THEN THE MOVEMENT WILL HIGHER
      *    CYLINDER TO LOWER CYLINDER
           IF WS-PREV > WS-START THEN
             PERFORM HILO
           ELSE
             PERFORM LOHI
           END-IF.
           STOP RUN.
      * HILO PARAGRAPH TO MOVE FROM HIGHER CYLINDER TO LOWER CYLINDER
      * THEN REACHING THE LOWEST REQUEST WILL REVERSE THE MOVEMENT
       HILO.
      *    LOOPING FROM THE STARTING INDEX TO THE LOWEST REQUEST
           PERFORM VARYING I FROM WS-START-IDX BY -1 UNTIL I < 2
      *      MOVING THE ELEMENTS TO THE P1S AND P2S
             MOVE WS-PROCESSES(I) TO WS-P1S
             MOVE WS-PROCESSES(I - 1) TO WS-P2S
      *      COMPUTING FOR THE HEAD MOVEMENT
             COMPUTE WS-HEAD-M = WS-P1S - WS-P2S
      *      DISPLAYING THE HEAD MOVEMENT
             DISPLAY "HEAD MOVEMENT: " WS-HEAD-M
      *      ADDING THE HEAD MOVEMENT TO THE TOTAL HEAD MOVEMENT
             COMPUTE WS-THM = WS-THM + WS-HEAD-M
           END-PERFORM.
           IF WS-START-IDX >= WS-NO-PROC THEN
             EXIT
           END-IF.
      *    LOOPING STARTING TO THE LOWEST REQUEST THEN NEXT THE STARTING
      *    INDEX TO THE HIGHEST REQUEST
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-NO-PROC - 1
      *    COMPUTING FOR THE HEAD MOVEMENT OF LOWEST INDEX TO THE NEXT
      *    INDEX OF THE STARTING INDEX
             IF I IS EQUAL TO 1 THEN
               MOVE WS-PROCESSES(WS-START-IDX + 1) TO WS-P1S
               MOVE WS-PROCESSES(I) TO WS-P2S
               MOVE WS-START-IDX TO I
      *    ELSE MOVING BY 1 ASCENDINGLY, COMPUTE FOR THE HEAD MOVEMENT
      *    UNTIL IT REACHES THE HIGHEST REQUEST
             ELSE
               MOVE WS-PROCESSES(I + 1) TO WS-P1S
               MOVE WS-PROCESSES(I) TO WS-P2S
             END-IF
      *    COMPUTING FOR THE HEAD MOVEMENT
             COMPUTE WS-HEAD-M = WS-P1S - WS-P2S
      *    DISPLAYING THE HEAD MOVEMENT
             DISPLAY "HEAD MOVEMENT: " WS-HEAD-M
      *    ADDING THE HAD MOVEMENT TO THE TOTAL HEAD MOVEMENT
             COMPUTE WS-THM = WS-THM + WS-HEAD-M
           END-PERFORM.
      *    DISPLAYING THE TOTAL HEAD MOVEMENT
           DISPLAY "THM: " WS-THM.
           EXIT.
      * LOHI PARAGRAPH TO MOVE FROM LOWER CYLINDER TO HIGHER CYLINDER
      * THEN REACHING THE HIGHEST REQUEST WILL REVERSE THE MOVEMENT
       LOHI.
      *    LOOPING FROM THE STARTING INDEX TO THE HIGHEST REQUEST
           PERFORM VARYING I FROM WS-START-IDX BY 1 UNTIL I >
           WS-NO-PROC - 1
      *      MOVING THE ELEMENTS TO THE P1S AND P2S
             MOVE WS-PROCESSES(I + 1) TO WS-P1S
             MOVE WS-PROCESSES(I) TO WS-P2S
      *      COMPUTING FOR THE HEAD MOVEMENT
             COMPUTE WS-HEAD-M = WS-P1S - WS-P2S
      *      DISPLAYING THE HEAD MOVEMENT
             DISPLAY "HEAD MOVEMENT: " WS-HEAD-M
      *      ADDING THE HEAD MOVEMENT TO THE TOTAL HEAD MOVEMENT
             COMPUTE WS-THM = WS-THM + WS-HEAD-M
           END-PERFORM.
           IF WS-START-IDX <= 1 THEN
             EXIT
           END-IF.
      *    LOOPING STARTING TO THE HIGHEST REQUEST THEN NEXT THE
      *    STARTING INDEX TO THE LOWEST REQUEST
           PERFORM VARYING I FROM WS-NO-PROC BY -1 UNTIL I <= 1
      *    COMPUTING FOR THE HEAD MOVEMENT OF HIGHEST INDEX TO THE NEXT
      *    INDEX OF THE STARTING INDEX
             IF I IS EQUAL TO WS-NO-PROC THEN
               MOVE WS-PROCESSES(I) TO WS-P1S
               MOVE WS-PROCESSES(WS-START-IDX - 1) TO WS-P2S
               MOVE WS-START-IDX TO I
      *    ELSE MOVING BY 1 DESCENDINGLY, COMPUTE FOR THE HEAD MOVEMENT
      *    UNTIL IT REACHES THE LOWEST REQUEST
             ELSE
               MOVE WS-PROCESSES(I) TO WS-P1S
               MOVE WS-PROCESSES(I - 1) TO WS-P2S
             END-IF
      *      COMPUTING FOR THE HEAD MOVEMENT
             COMPUTE WS-HEAD-M = WS-P1S - WS-P2S
      *      DISPLAYING THE HEAD MOVEMENT
             DISPLAY "HEAD MOVEMENT: " WS-HEAD-M
      *      ADDING THE HEAD MOVEMENT TO THE TOTAL HEAD MOVEMENT
             COMPUTE WS-THM = WS-THM + WS-HEAD-M
           END-PERFORM.
      *    DISPLAYING THE TOTAL HEAD MOVEMENT
           DISPLAY "THM: " WS-THM.
           EXIT.
      * BUBBLE SORT PARAGRAPH
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
      * FIND START PARAGRAPH
       FIND-START.
      *    LOOPING TO FIND THE STARTING HEAD OF THE LOOK ALGORITHM
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= WS-NO-PROC
             IF WS-PROCESSES(I) IS EQUAL TO WS-START
               MOVE I TO WS-START-IDX
             END-IF
           END-PERFORM.
           EXIT.
       END PROGRAM LOOK-DISK-ALGORITHM.
