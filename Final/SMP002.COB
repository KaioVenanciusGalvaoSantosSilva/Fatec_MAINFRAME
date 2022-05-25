       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADCID.
      *AUTHOR. KAIO V G S S.
      **************************************
      * MANUTENCAO DO CADASTRO DE CIDS   *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCID ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS DENOMINACAO WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCID
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCID.DAT". 
       01 REGCID.
          03 CODIGO			PIC 9(06).
          03 DENOMINACAO    PIC X(30).
          03 OBSER       	PIC X(30). 
		  03 DATACID.
 		 	05 NUMERODD 	PIC 9(02).
			05 NUMEROMM 	PIC 9(02).
			05 NUMEROAA 	PIC 9(04).

          
      *Defini a data como algo A+ e OBS.
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      	PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       01 ST-ERRO      	PIC X(02) VALUE "00".
       01 W-SEL             PIC 9(01) VALUE ZEROS.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA2.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "                            ".
           05  LINE 01  COLUMN 41 
               VALUE  " ".
           05  LINE 02  COLUMN 01 
               VALUE                            "  CADASTRO DE CID's".
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
               VALUE  "    DENOMINACAO : ".
           05  LINE 11  COLUMN 01 
               VALUE  "    OBSERVACOES :  ".
           05  LINE 25  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TCODIGO
               LINE 05  COLUMN 15  PIC 9(06)
               USING  CODIGO
               HIGHLIGHT.
           05  TDENOMINACAO
               LINE 08  COLUMN 20  PIC X(30)
               USING  DENOMINACAO
               HIGHLIGHT.
           05  TOBSER
               LINE 11  COLUMN 20  PIC X(30)
               USING  OBSER
               HIGHLIGHT.   			   
           05  TDATACID
               LINE 14  COLUMN 11  PIC X(10)
               USING  DATACID
               HIGHLIGHT.
           05  LINE 14  COLUMN 01 
               VALUE  "    DATA :  /  /    ".			   
           05  TNUMERODD
               LINE 14  COLUMN 11  PIC 9(02)
               USING  NUMERODD.		
           05  TNUMEROMM
               LINE 14  COLUMN 14  PIC 9(02)
               USING  NUMEROMM.	
           05  TNUMEROAA
               LINE 14  COLUMN 17  PIC 9(04)
               USING  NUMEROAA.	 
      *
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.

       R0.
           OPEN I-O CADCID
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCID
                 CLOSE CADCID
                 MOVE "*** ARQUIVO CADCID FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADCID" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *                
       R1.
           MOVE SPACES TO  DENOMINACAO  OBSER
           MOVE ZEROS TO CODIGO DATACID
		   MOVE ZEROS TO NUMERODD NUMEROMM NUMEROAA
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
				   				   
       LER-CADCIDCOD.
           READ CADCID
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
       
				PERFORM R3A
				PERFORM R4A
				PERFORM R5A	
				
                DISPLAY TELA2
                MOVE "*** CID JA CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CID" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** CID NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.

       R3.
		DISPLAY TELA2
           ACCEPT TDENOMINACAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
		DISPLAY TELA2.
       R3A.		
		IF DENOMINACAO = SPACES
               MOVE "*Insira a Denominação*" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.		 
		DISPLAY TDENOMINACAO.
		DISPLAY TELA2.
		
       R4.
		DISPLAY TELA2
           ACCEPT TOBSER
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
		DISPLAY TELA2.
		
       R4A.		
		IF OBSER = SPACES
               MOVE "*Insira ALGUMA OBSERVAÇÃO*" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.		 
		DISPLAY TOBSER.
		DISPLAY TELA2.

       R5.	   
		DISPLAY TELA2

			ACCEPT TNUMERODD.
			ACCEPT TNUMEROMM.
			ACCEPT TNUMEROAA.
			ACCEPT W-ACT FROM ESCAPE KEY
			IF W-ACT = 01
		GO TO R4.
       R5A.
		IF NUMERODD=ZEROS OR NUMEROMM=ZEROS OR NUMEROAA=ZEROS
               MOVE "*Insira a DATA corretamente*" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.
		IF NUMERODD>31 OR NUMEROMM>12 OR NUMEROAA<0
               MOVE "*Insira a DATA corretamente2*" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.
		IF NUMEROMM=02 AND NUMERODD >29
		   MOVE "*Insira a DATA corretamente3*" TO MENS
		   PERFORM ROT-MENS THRU ROT-MENS-FIM
		  GO TO R5.

			DISPLAY TNUMERODD.
			DISPLAY TNUMEROMM.
			DISPLAY TNUMEROAA.
		DISPLAY TELA2.	

 
      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (25, 40) "DADOS OK (S/N) : ".
                ACCEPT (25, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R5.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCID
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "*CID JA EXISTE,DADOS NAO GRAVADOS*" TO MENS
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
                DELETE CADCID RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO CID EXCLUIDO ***" TO MENS
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
                REWRITE REGCID
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CID"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADCID.
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
      

 