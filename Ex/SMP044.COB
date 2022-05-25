       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP044.
      **************************************************
      * CONSULTA DE PACIENTE ********************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       SELECT CADPACI ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CPF
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME
                                   WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *
       FD CADPACI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPACI.DAT".
       01 REGPACI.
          03 CPF            PIC 9(04).
          03 NOMEP           PIC X(30).
          03 SEXO           PIC X(01).
          03 DATANASC       PIC 9(08).
          03 GENERO         PIC X(01).
          03 CONVENIO       PIC 9(04).
          03 CEPPACI        PIC 9(08).  
          03 NUMENDE        PIC 9(04).
          03 COMPLEM        PIC X(10).
          03 TELEFONE       PIC 9(11).
          03 EMAIL          PIC X(30).	
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-SEL             PIC 9(01) VALUE ZEROS.
       01 W-CONT         PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC X(01) VALUE SPACES.
       01 ST-ERRO        PIC X(02) VALUE "00".
       01 W-ACT            PIC 9(02) VALUE ZEROS.
       01 MENS             PIC X(50) VALUE SPACES.
       01 LIMPA             PIC X(55) VALUE SPACES.
       01 SOLIC             PIC X(20) VALUE SPACES.
       01 CONLIN          PIC 9(03) VALUE 001.
       01 CEPENTR      PIC 9(09) VALUE ZEROS.
       01 LOGRENTR    PIC X(30) VALUE SPACES.
      *-----------------------------------------------------------------
      ******************
      * ROTINA DE TELA *
      ******************
      *
       SCREEN SECTION.
       01  SMP044.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                             CONSULTA DE".
           05  LINE 02  COLUMN 41 
               VALUE  " PACIENTE".
           05  LINE 05  COLUMN 01 
               VALUE  "    NOME         :".
           05  LINE 07  COLUMN 01 
               VALUE  "    CEP  :".
           05  LINE 09  COLUMN 01 
               VALUE  "    NASCIMENTO      :".
           05  LINE 11  COLUMN 01 
               VALUE  "    GENERO      :".
           05  LINE 13  COLUMN 01 
               VALUE  "    CONVENIO          :".
           05  LINE 15  COLUMN 01 
               VALUE  "    TELEFONE          :".
           05  LINE 17  COLUMN 01 
               VALUE  "    EMAIL          :".
           05  LINE 21  COLUMN 01 
               VALUE  "              OPCAO :   (E=ENCERRA     P".
           05  LINE 21  COLUMN 41 
               VALUE  "=PROXIMO     A=ANTERIOR )".
           05  LINE 22  COLUMN 41 
               VALUE  "   ==>            <==".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT  CADPACI
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADPACI NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO CADPACI"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
      *
       INC-001.
           MOVE ZEROS TO PACI-NUMPACI
           MOVE SPACES TO PACI-LOGRA PACI-BAIRRO PACI-CIDADE PACI-UF
           DISPLAY  SMP044.
       INC-001A.
           ACPACIT TPACI-NUMPACI
           ACPACIT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE CADPACI
                      GO TO ROT-FIM.
           IF W-ACT > 02
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
        
           START CADPACI KEY IS NOT LESS PACI-NUMPACI INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.
       INC-RD2.
           READ CADPACI NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       ROT-MONTAR.
              DISPLAY SMP044.
       ROT-SOL.

           ACPACIT  TW-OPCAO  
           IF W-OPCAO = "E"
                  GO TO ROT-FIM 
           ELSE
             IF W-OPCAO = "P"
                   GO TO INC-RD2
             ELSE
                
                     MOVE "*** OPCAO NAO DISPONIVEL ***" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO ROT-SOL.

      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.

           CLOSE CADPACI.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 1000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.
      *

      *---------------------*** FIM DE PROGRAMA ***--------------------*