      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.

       WORKING-STORAGE SECTION.


       01 STRINGA2 PIC A(13) VALUE 'DamAenico'.
       01  VAL PIC A(13).
       01 PAROLA PIC A(14).
       *>CONTATORI

       01 CONTATORE_B PIC 9(5) VALUE 0.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "INSERISCI PAROLA:".
           ACCEPT PAROLA.


           PERFORM CONTA-LETTERA.

           STOP RUN.


       CONTA-LETTERA.

           INSPECT PAROLA TALLYING CONTATORE_B FOR ALL 'a'  *> QUI CONTA SOLO IL CARATTERE A
           DISPLAY CONTATORE_B.
