       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADCONV.
      *AUTHOR. KAIO V G S S.
      **************************************
      * MANUTENCAO DO CADASTRO DE CONVENIOS *
      **************************************
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
					ALTERNATE RECORD KEY IS PLANO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT". 
       01 REGCONV.
          03 CODIGO			PIC 9(06).
          03 NOMEC          	PIC X(30).
		  03 PLANO			PIC 9(02).
		  03 DATAVALID.
 		 	05 NUMDD 	    PIC 9(02).
			05 NUMMM 	    PIC 9(02).
			05 NUMAA 	    PIC 9(04).		
		  
      *Defini a data de nascimento dessa forma como algo A+ e OBS.
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(04) VALUE ZEROS.
       77 W-OPCAO      	PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       01 ST-ERRO      	PIC X(02) VALUE "00".
	   01 TXTPLANO    	PIC X(30) VALUE SPACES.
       01 W-SEL             PIC 9(01) VALUE ZEROS.	   

	   01 TABCONVENIO.
          03 TBCONVENIO    PIC X(20) OCCURS 12 TIMES.	
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA2.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "                            ".
           05  LINE 01  COLUMN 41 
               VALUE  " ".
           05  LINE 02  COLUMN 01 
               VALUE                            " CADASTRO DE CONVENIO".
           05  LINE 03  COLUMN 01 
               VALUE  "                            ".
           05  LINE 03  COLUMN 41 
               VALUE  " ".
           05  LINE 04  COLUMN 01 
               VALUE  "  ".
           05  LINE 05  COLUMN 01 
               VALUE  "    CODIGO :          ".
           05  LINE 06  COLUMN 01 
               VALUE  "  ".
           05  LINE 08  COLUMN 01 
               VALUE  "    NOMEC : ".
           05  LINE 11  COLUMN 01 
               VALUE  "    PLANO :  ".
           05  LINE 25  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TCODIGO
               LINE 05  COLUMN 15  PIC 9(04)
               USING  CODIGO
               HIGHLIGHT.
           05  TNOMEC
               LINE 08  COLUMN 15  PIC X(30)
               USING  NOMEC
               HIGHLIGHT.
           05  TPLANO
               LINE 11  COLUMN 16  PIC 9(02)
               USING  PLANO
               HIGHLIGHT. 
		   05  TTXTPLANO
			   LINE 11  COLUMN 25  PIC X(30)
			   USING  TXTPLANO
			   HIGHLIGHT.	  			   
           05  TDATAVALID
               LINE 14  COLUMN 25  PIC X(10)
               USING  DATAVALID
               HIGHLIGHT.
           05  LINE 14  COLUMN 01 
               VALUE  "    DATA DE VALIDADE :    /  /    ".			   
           05  TNUMDD
               LINE 14  COLUMN 25  PIC 9(02)
               USING  NUMDD.		
           05  TNUMMM
               LINE 14  COLUMN 28  PIC 9(02)
               USING  NUMMM.	
           05  TNUMAA
               LINE 14  COLUMN 31  PIC 9(04)
               USING  NUMAA.	 
      *
	    01  TELATA.
           05  LINE 05  COLUMN 50 
               VALUE  "01-ENFERMARIA REGINAL ".
           05  LINE 06  COLUMN 50 
               VALUE  "02-ENFERMARIA NACIONAL".
           05  LINE 07  COLUMN 50 
               VALUE  "03-ENFERMARIA INTERNACIONAL".
           05  LINE 08  COLUMN 50 
               VALUE  "04-APTO PADRÃO REGINAL ".
           05  LINE 09  COLUMN 50 
               VALUE  "05-APTO PADRAO NACIONAL".
           05  LINE 10  COLUMN 50 
               VALUE  "06-APTO PADRAO INTERNACIONAL".
           05  LINE 11  COLUMN 50 
               VALUE  "07-EMERGENCIA REGIONAL".
           05  LINE 12  COLUMN 50 
               VALUE  "08-EMERGENCIA NACIONAL".
           05  LINE 13  COLUMN 50 
               VALUE  "09-EMERCENCIA INTERNACIONAL".
           05  LINE 14  COLUMN 50 
               VALUE  "10-PLANO GLOBAL ".			   
           05  LINE 15  COLUMN 50 
               VALUE  "11-INTERPLANETARIO".
           05  LINE 16  COLUMN 50 
               VALUE  "12-DEEPWEB".			   
			   
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "ENFERMARIA REGINAL" TO TBCONVENIO(01)
           MOVE "ENFERMARIA NACIONAL" TO TBCONVENIO(02)
           MOVE "ENFERMARIA INTERNACIONAL"    TO TBCONVENIO(03)
           MOVE "APTO PADRÃO REGINAL"   TO TBCONVENIO(04)
           MOVE "APTO PADRAO NACIONAL"   TO TBCONVENIO(05)
           MOVE "APTO PADRAO INTERNACIONAL"   TO TBCONVENIO(06)
           MOVE "EMERGENCIA REGIONAL"    TO TBCONVENIO(07)
           MOVE "EMERGENCIA NACIONAL"        TO TBCONVENIO(08)
           MOVE "EMERCENCIA INTERNACIONAL"  TO TBCONVENIO(09).
		   MOVE "PLANO GLOBAL"  TO TBCONVENIO(10).
		   MOVE "INTERPLANETARIO"  TO TBCONVENIO(11).
		   MOVE "DEEPWEB"  TO TBCONVENIO(12).		   
       R0.
           OPEN I-O CADCONV
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCONV
                 CLOSE CADCONV
                 MOVE "*** ARQUIVO CADCONV FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADCONV" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *                
       R1.
           MOVE SPACES TO  NOMEC DATAVALID  TXTPLANO
           MOVE ZEROS TO CODIGO PLANO
		   MOVE ZEROS TO NUMDD NUMMM NUMAA
           DISPLAY TELA2.
       R2.
		DISPLAY TELA2
		   ACCEPT TCODIGO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
       R2A.
		IF CODIGO = ZEROS
		   MOVE "*Insira o CODIGO*" TO MENS
		   PERFORM ROT-MENS THRU ROT-MENS-FIM
		  GO TO R2.				  
		
		DISPLAY TCODIGO.
		DISPLAY TELA2.				   
				   				   
       LER-CADCONVIDADO.
           READ CADCONV
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
       
				PERFORM R3A
				PERFORM R4A
				PERFORM R5A	
				
                DISPLAY TELA2
                MOVE "*** CONVENIO JA CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CONVENIO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** CONVENIO NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3.
		DISPLAY TELA2
           ACCEPT TNOMEC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
		DISPLAY TELA2.
       R3A.		
		IF NOMEC = SPACES
               MOVE "*Insira o NOME*" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.		 
		DISPLAY TNOMEC.
		DISPLAY TELA2.
		
       R4.
           DISPLAY TELATA
			ACCEPT TPLANO.
			ACCEPT W-ACT FROM ESCAPE KEY
			IF W-ACT = 01
                   GO TO R3.
		DISPLAY TELA2.
       R4A.		

		DISPLAY TELA2
           IF PLANO = 0 OR > 12
                 MOVE "* DIGITE APENAS DE 01 ATE 12 *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4.
 
           MOVE TBCONVENIO(PLANO) TO TXTPLANO
           DISPLAY TTXTPLANO.
		DISPLAY TELA2.

       R5.	   
		DISPLAY TELA2

			ACCEPT TNUMDD.
			ACCEPT TNUMMM.
			ACCEPT TNUMAA.
			ACCEPT W-ACT FROM ESCAPE KEY
			IF W-ACT = 01
		GO TO R4.
       R5A.
		IF NUMDD=ZEROS OR NUMMM=ZEROS OR NUMAA=ZEROS
               MOVE "*Insira a DATA corretamente*" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.
		IF NUMDD>31 OR NUMMM>12 OR NUMAA<0
               MOVE "*Insira a DATA corretamente2*" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.
		IF NUMMM=02 AND NUMDD >29
		   MOVE "*Insira a DATA corretamente3*" TO MENS
		   PERFORM ROT-MENS THRU ROT-MENS-FIM
		  GO TO R5.

			DISPLAY TNUMDD.
			DISPLAY TNUMMM.
			DISPLAY TNUMAA.
		DISPLAY TELA2.	

      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (25, 40) "DADOS OK (S/N) : ".
                ACCEPT (25, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R5
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCONV
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "*CONVENIO JA EXISTE,DADOS NAO GRAVADOS*" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (25, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (25, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (25, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3.
      *
       EXC-OPC.
                DISPLAY (25, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (25, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADCONV RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO CONVENIO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (25, 40) "ALTERAR  (S/N) : ".
                ACCEPT (25, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R5.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCONV
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CONVENIO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADCONV.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (25, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (25, 12) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

      *    FILE STATUS
      *    00 = OPERAÇÃO REALIZADO COM SUCESSO
      *    10 = FIM DE ARQUIVO ( QUANDO EXECUTADO LEITURA SEQUENCIAL ) 
      *    22 = REGISTRO JÁ CADASTRADO
      *    23 = REGISTRO NÃO ENCONTRADO
      *    30 = ARQUIVO NÃO ENCONTRADO
      *    95 = ISAM NÃO CARREGADO
      

