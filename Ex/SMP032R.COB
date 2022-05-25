       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP032.
       AUTHOR. KAIO V G S S
      ***********************************************
      * CONVERCAO DO ARQUIVO CADCONV PARA TXT        *
      ***********************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOMEC
                                   WITH DUPLICATES.
           SELECT CADCONVTX ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT".
       01 REGCONV.
          03 CODIGO			PIC 9(06).
          03 NOMEC          PIC X(30).
		  03 PLANO			PIC 9(02).
      *
       FD CADCONVTX
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONVRR.TXT".
       01 REGCONVTX    PIC X(100).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL      PIC 9(01) VALUE ZEROS.
       77 W-CONT     PIC 9(06) VALUE ZEROS.
       77 W-OPCAO    PIC X(01) VALUE SPACES.
       77 ST-ERRO    PIC X(02) VALUE "00". 
       77 W-ACT      PIC 9(02) VALUE ZEROS.
       77 MENS       PIC X(50) VALUE SPACES.
       77 LIMPA      PIC X(50) VALUE SPACES.
      *
       01 CONVTX.
                03 CODIGOTX      PIC 9(08).
                03 FILLER        PIC X(01) VALUE " ".
                03 NOMECTX       PIC X(30).
                03 FILLER        PIC X(01) VALUE " ".
                03 PLANOTX       PIC X(20).

       01 CAB0.
                03 FILLER      PIC X(20) VALUE "SISTEMA MEDICO".
                03 FILLER      PIC X(30) VALUE
                        "   *** RELACAO DE CADCONV ***".

       01 CAB1.
                03 CAB-CODIGOTX      PIC X(08) VALUE "  COD   ".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-NOMECTX       PIC X(30) VALUE "   CONVENIO".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-PLANOTX      PIC X(20) VALUE "   PLANO".

       01 CAB2.
                03 CAB-CODIGOTX      PIC X(08) VALUE "  ---   ".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-NOMECTX       PIC X(30) VALUE "   ----------".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-PLANOTX      PIC X(20) VALUE "   ------".

      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  SMT032.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                *** CONVERSAO DE ARQUIVO".
           05  LINE 02  COLUMN 41 
               VALUE  " DE CADCONV PARA TXT ***".
           05  LINE 12  COLUMN 01 
               VALUE  "                         CONFIRMA CONVER".
           05  LINE 12  COLUMN 41 
               VALUE  "SAO (S ou N ) :".
           05  LINE 24  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TW-OPCAO
               LINE 12  COLUMN 57  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO. 
      *
       INC-001.
                MOVE SPACES TO NOMEC .
                MOVE ZEROS  TO CODIGO PLANO.
                DISPLAY SMT032.
       INC-OPC.
                ACCEPT TW-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
      *
       INC-OP0.
           OPEN INPUT CADCONV
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO CADCONV NAO EXISTE *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADCONV" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
      *
           OPEN OUTPUT CADCONVTX
           IF ST-ERRO NOT = "00"
                      MOVE "ERRO ABERTURA DO ARQUIVO CADCONVTX" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
           WRITE REGCONVTX FROM CAB0.
           MOVE SPACES TO REGCONVTX
           WRITE REGCONVTX.
           WRITE REGCONVTX FROM CAB1.
           WRITE REGCONVTX FROM CAB2.
           MOVE SPACES TO REGCONVTX
           WRITE REGCONVTX.
      *
       LER-conv01.
                READ CADCONV NEXT
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "10"
                      MOVE "*** FIM DO CADCONV ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA LEITURA CADCONV"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
           MOVE CODIGO TO CODIGOTX.
           MOVE NOMEC  TO NOMECTX.
           MOVE PLANO TO PLANOTX.

           MOVE CONVTX      TO REGCONVTX.
       INC-WR1.
                WRITE REGCONVTX
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO LER-conv01
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO CADCONVTX"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                CLOSE CADCONV CADCONVTX.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
