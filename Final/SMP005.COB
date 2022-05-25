       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADCEP.
      *AUTHOR. KAIO V G S S.
      **************************************
      * MANUTENCAO DO CADASTRO DO CEP   *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS ENDERECO WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT". 
       01 REGCEP.
          03 CEP                PIC 9(08).
          03 ENDERECO          	PIC X(30).
		  03 BAIRRO  			PIC X(20).
          03 CIDADE        		PIC X(20).
          03 ESTADO        		PIC X(02).
          
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      	PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       01 ST-ERRO      	PIC X(02) VALUE "00".
       01 W-SEL             PIC 9(01) VALUE ZEROS.	   
	   77 UF          	PIC X(50) VALUE SPACES.
	   
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA2.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "                            ".
           05  LINE 01  COLUMN 41 
               VALUE  " ".
           05  LINE 02  COLUMN 01 
               VALUE                            "  CADASTRO DE CEP".
           05  LINE 03  COLUMN 01 
               VALUE  "                            ".
           05  LINE 03  COLUMN 41 
               VALUE  " ".
           05  LINE 04  COLUMN 01 
               VALUE  "  ".
           05  LINE 06  COLUMN 01 
               VALUE  "  ".
           05  LINE 08  COLUMN 01 
               VALUE  "    BAIRRO: ".
           05  LINE 11  COLUMN 01 
               VALUE  "    CIDADE :                      ".
           05  LINE 25  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TCEP
               LINE 05  COLUMN 10  PIC 99999.999
               USING  CEP
               HIGHLIGHT.
           05  LINE 05  COLUMN 01 
               VALUE  "    CEP :".
           05  LINE 05  COLUMN 28 
               VALUE  "    ENDERECO :".           		   
           05  TENDERECO
               LINE 05  COLUMN 43  PIC X(30)
               USING  ENDERECO
               HIGHLIGHT.
           05  TBAIRRO
               LINE 08  COLUMN 14  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 11  COLUMN 14  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  LINE 13  COLUMN 01 
               VALUE  "    ESTADO :  ".				   
           05  TESTADO
               LINE 13  COLUMN 14  PIC X(02)
               USING  ESTADO
               HIGHLIGHT.
		   05  TUF
               LINE 13  COLUMN 18  PIC X(20)
               USING  UF
               HIGHLIGHT.
			   
      *
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
       
       R0.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCEP
                 CLOSE CADCEP
                 MOVE "*** ARQUIVO CADCEP FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADCEP" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *                
       R1.
           MOVE SPACES TO  ENDERECO ESTADO  
		   MOVE SPACES TO  BAIRRO CIDADE UF
		   MOVE ZEROS TO   CEP
           DISPLAY TELA2.
       R2.
		DISPLAY TELA2
           ACCEPT TCEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
		DISPLAY TELA2.
       R2A.		
		IF CEP = SPACES
               MOVE "*Insira o CEP*" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.		 
		DISPLAY TCEP.
		DISPLAY TELA2.	
		
       LER-CADCEP.
           READ CADCEP
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
       
				PERFORM R4A
                PERFORM R6A
				
                DISPLAY TELA2
                MOVE "*** CEP JA CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CEP" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** CEP NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.

       R3.
		DISPLAY TELA2
           ACCEPT TENDERECO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
		DISPLAY TELA2.
       R3A.		
		IF ENDERECO = SPACES
               MOVE "*Insira um Endereco*" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.		 
		DISPLAY TENDERECO.
		DISPLAY TELA2.		
		
       R4.	   
		DISPLAY TELA2
           ACCEPT TBAIRRO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
		DISPLAY TELA2.
       R4A.		
		IF BAIRRO = SPACES
               MOVE "*Insira um Bairro*" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.		 
		DISPLAY TBAIRRO.
		DISPLAY TELA2.
		   
       R5.
		DISPLAY TELA2
           ACCEPT TCIDADE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
		DISPLAY TELA2.
       R5A.		
		IF CIDADE = SPACES
               MOVE "*Insira um Cidade*" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.		 
		DISPLAY TCIDADE.
		DISPLAY TELA2.	
		 
       R6.	   
		DISPLAY TELA2
           ACCEPT TESTADO
		   DISPLAY UF
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
		DISPLAY TELA2.
       R6A.		
		IF ESTADO = SPACES
               MOVE "*Insira a sigla de um ESTADO*" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.	

		IF ESTADO = 'RJ' OR 'rj'
				MOVE 'RIO DE JANEIRO' TO UF
		ELSE
			IF ESTADO = 'SP' OR 'sp'
				MOVE 'SAO PAULO' TO UF	
		ELSE				
		IF ESTADO = 'MG' OR 'mg'
				MOVE 'MINAS GERAIS' TO UF	
		ELSE				
		IF ESTADO = 'AC' OR 'ac'
				MOVE 'ACRE' TO UF	
		ELSE
			IF ESTADO = 'AL' OR 'al'
				MOVE 'ALAGOAS' TO UF	
		ELSE
			IF ESTADO = 'AP' OR 'ap'
				MOVE 'AMAPA' TO UF	
		ELSE
			IF ESTADO = 'AM' OR 'am'
				MOVE 'AMAZONAS' TO UF	
		ELSE				
			IF ESTADO = 'BA' OR 'ba'
				MOVE 'BAHIA' TO UF	
		ELSE
			IF ESTADO = 'CE' OR 'ce'
				MOVE 'CEARA' TO UF	
		ELSE
			IF ESTADO = 'DF' OR 'df'
				MOVE 'DISTRITO FEDERAL' TO UF	
		ELSE				
			IF ESTADO = 'ES' OR 'es'
				MOVE 'ESPIRITO SANTO' TO UF	
		ELSE
			IF ESTADO = 'GO' OR 'go'
				MOVE 'GOIAS' TO UF	
		ELSE	
			IF ESTADO = 'MA' OR 'ma'
				MOVE 'MARANHAO' TO UF	
		ELSE
			IF ESTADO = 'MT' OR 'mt'
				MOVE 'MATO GROSSO' TO UF	
		ELSE
			IF ESTADO = 'MS' OR 'ms'
				MOVE 'MATO GROSSO DO SUL' TO UF	
		ELSE
			IF ESTADO = 'PA' OR 'pa'
				MOVE 'PARA' TO UF	
		ELSE				
			IF ESTADO = 'PB' OR 'pb'
				MOVE 'PARAIBA' TO UF	
		ELSE
			IF ESTADO = 'PR' OR 'pr'
				MOVE 'PARAIBA' TO UF	
		ELSE
			IF ESTADO = 'PE' OR 'pe'
				MOVE 'PERNAMBUCO' TO UF	
		ELSE	
			IF ESTADO = 'PI' OR 'pi'
				MOVE 'PIAUI' TO UF	
		ELSE	
			IF ESTADO = 'RN' OR 'rn'
				MOVE 'RIO GRANDE DO NORTE' TO UF	
		ELSE	
			IF ESTADO = 'RS' OR 'rs'
				MOVE 'RIO GRANDE DO SUL' TO UF	
		ELSE
			IF ESTADO = 'RO' OR 'ro'
				MOVE 'RONDONIA' TO UF	
		ELSE	
			IF ESTADO = 'RR' OR 'rr'
				MOVE 'RORAIMA' TO UF	
		ELSE
			IF ESTADO = 'SC' OR 'sc'
				MOVE 'SANTA CATARINA' TO UF	
		ELSE		
			IF ESTADO = 'SE' OR 'se'
				MOVE 'SERGIPE' TO UF	
		ELSE
			IF ESTADO = 'TO' OR 'to'
				MOVE 'TOCANTINS' TO UF	
		ELSE
			     MOVE "* INSIRA UM ESTADO VALIDO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R6.
				
	
		DISPLAY TESTADO.
		DISPLAY TELA2.	

      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (25, 40) "DADOS OK (S/N) : ".
                ACCEPT (25, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R6.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "*PACIENTE JA EXISTE,DADOS NAO GRAVADOS*" TO MENS
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
                DELETE CADCEP RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO PACIENTE EXCLUIDO ***" TO MENS
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
                IF W-ACT = 01 GO TO R6.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO PACIENTE"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADCEP.
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
      

