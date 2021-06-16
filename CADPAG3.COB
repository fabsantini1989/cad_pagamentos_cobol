       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDF222.
       AUTHOR. FABIANO SANTINI MARQUES.
      ***********************************************
      *    MANUTENCAO FOLHA DE PAGAMENTO   *
      ***********************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADPAG ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS COD
                    FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADPAG
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPAG.DAT".
       01 REGPAG.
                03 COD                 PIC 9(06).
                03 NOME                PIC X(30).
                03 SB                  PIC 9(08).
				03 DP                  PIC 9(03).
				03 CG                  PIC X(20).
                03 IR                  PIC 9(08).
                03 INSS                PIC 9(08).
				03 SL                  PIC 9(08).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL          PIC 9(01) VALUE ZEROS.
       77 W-CONT         PIC 9(06) VALUE ZEROS.
       77 W-OPCAO        PIC X(01) VALUE SPACES.
       77 ST-ERRO        PIC X(02) VALUE "00". 
       77 W-ACT          PIC 9(02) VALUE ZEROS.
       77 MENS           PIC X(50) VALUE SPACES.
       77 LIMPA          PIC X(50) VALUE SPACES.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELAPAG REQUIRED BACKGROUND-COLOR 7.
	       05 BACKGROUND-COLOR 7.
           05  BLANK SCREEN.
           05  LINE 24  COLUMN 01 
               VALUE  " _______________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.
           05  LINE 24  COLUMN 41 
               VALUE  "_______________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.
           05  LINE 25  COLUMN 01 
               VALUE  "                                        "
			   BACKGROUND-COLOR 1
			   FOREGROUND-COLOR  6.			   
           05  LINE 25  COLUMN 41 
               VALUE  "             * SANTINI SOLUTIONS (C) *  "
			   BACKGROUND-COLOR 1
			   FOREGROUND-COLOR  15.
           05  LINE 01  COLUMN 01 
               VALUE  "                                        "
			   BACKGROUND-COLOR  1.
           05  LINE 01  COLUMN 41 
               VALUE  "                                        "
			   BACKGROUND-COLOR  1.		   
           05  LINE 02  COLUMN 01 
               VALUE  "             S I S T E M A  D E  F O L H"
			   HIGHLIGHT
			   FOREGROUND-COLOR  15.
           05  LINE 02  COLUMN 41 
               VALUE  " A  D E  P A G A M E N T O              ".
           05  LINE 03  COLUMN 01 
               VALUE  " _______________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.
           05  LINE 03  COLUMN 41 
               VALUE  "________________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.
           05  LINE 04  COLUMN 01 
               VALUE  " _______________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.
           05  LINE 04  COLUMN 41 
               VALUE  "________________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.
           05  LINE 05  COLUMN 01 
               VALUE  "                                        "
			   BACKGROUND-COLOR  7.	
           05  LINE 05  COLUMN 41 
               VALUE  "                                        "
			   BACKGROUND-COLOR  7.			   
           05  LINE 06  COLUMN 32 
               VALUE  "COD :"
			   FOREGROUND-COLOR  6.
           05  LINE 07  COLUMN 01 
               VALUE  " _______________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.
           05  LINE 07  COLUMN 41 
               VALUE  "________________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.			   
           05  LINE 09  COLUMN 01 
               VALUE  " NOME :"
			   FOREGROUND-COLOR  0.	
           05  LINE 11  COLUMN 01 
               VALUE  " DP :"
			   FOREGROUND-COLOR  0.
           05  LINE 11  COLUMN 11 
               VALUE  " CARGO :"
			   FOREGROUND-COLOR  0.
           05  LINE 12  COLUMN 01 
               VALUE  " SALARIO BRUTO :"
			   FOREGROUND-COLOR  0.
           05  LINE 13  COLUMN 01 
               VALUE  " INSS :"
			   FOREGROUND-COLOR  0.
           05  LINE 14  COLUMN 01 
               VALUE  " IRRF :"
			   FOREGROUND-COLOR  0.
           05  LINE 15  COLUMN 01 
               VALUE  " SALARIO LIQUIDO :"
			   FOREGROUND-COLOR  0.
           05  LINE 16  COLUMN 01 
               VALUE  " "			   
			   FOREGROUND-COLOR  0.
           05  LINE 17  COLUMN 01 
               VALUE  " "
			   FOREGROUND-COLOR  0.
           05  LINE 18  COLUMN 01 
               VALUE  " "
			   FOREGROUND-COLOR  0.
           05  LINE 19  COLUMN 01 
               VALUE  " _______________________________________"
			   HIGHLIGHT
			   FOREGROUND-COLOR  0.
           05  LINE 19  COLUMN 41 
               VALUE  "_______________________________________".
           05  TCOD
               LINE 06  COLUMN 38  PIC 9(06)
               USING  COD
			   FOREGROUND-COLOR  6.
           05  TNOME
               LINE 09  COLUMN 09  PIC X(30)
               USING  NOME
			   FOREGROUND-COLOR  0.
           05  TDP
               LINE 11  COLUMN 07  PIC 9(03)
               USING  DP
			   FOREGROUND-COLOR  0.			
           05  TCG
               LINE 11  COLUMN 21  PIC X(20)
               USING  CG
			   FOREGROUND-COLOR  0.				   
           05  TSB
               LINE 12  COLUMN 17  PIC 999.999,99
               USING  SB.			   
           05  TINSS
               LINE 13  COLUMN 09  PIC 999.999,99
               USING  INSS.
           05  TIR
               LINE 14  COLUMN 09  PIC 999.999,99
               USING  IR.		   
           05  TSL
               LINE 15  COLUMN 20  PIC 999.999,99
               USING  SL.
      *	   
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *------------[ ROTINA DE ABERTURA ARQUIVO ]------------------       
       R0.
           OPEN I-O CADPAG
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADPAG
                 CLOSE CADPAG
                 MOVE "*** ARQUIVO CADPAG FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXCUTADO **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    MOVE "ERRO NA ABERTURA DO ARQUIVO CADPAG" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
      *------------[ INICIALIZACAO DAS VARIAVEIS ]---------------------               
       R1.
           MOVE SPACES TO NOME CG
		   MOVE ZEROS  TO COD DP SB SL IR INSS.
      *-------------[VISUALIZACAO DA TELA]--------------------------------
           DISPLAY TELAPAG.
      *-------------[ ENTRADA DA COD ]--------------------------------
       R2.
           DISPLAY (20, 29) "TECLAR ESC PARA SAIR"
           ACCEPT TCOD
           DISPLAY (20, 29) "                      "
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   CLOSE CADPAG
                   GO TO ROT-FIM.
           IF W-ACT = 02
                     MOVE "*** TECLEI O F1 **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO ROT-FIM.
           IF W-ACT = 10
                     MOVE "*** TECLEI O F9 **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R2.
           IF COD = ZEROS
                     MOVE "*** DADO NAO PODE FICAR VAZIO **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R2.   
       LER-CADMPAG.
           MOVE 0 TO W-SEL
           READ CADPAG
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00" OR "02"
                DISPLAY TELAPAG
                MOVE "*** COD JA CADASTRADA ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADPAG" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** COD NAO CADASTRADA ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3.       
           ACCEPT TNOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1.
       R4.
           ACCEPT TDP.  
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
       R5.
           ACCEPT TCG.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.			   				   
       R6.
           ACCEPT TSB.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.      
       R7.
           ACCEPT TINSS.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6.
       R8.
           ACCEPT TIR.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R7.
       R9.
           ACCEPT TSL.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R8.			   				   
      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (20, 29) "GRAVAR (S/N) : ".
                ACCEPT (20, 46) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R1.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGPAG
                IF ST-ERRO = "00"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU
					  ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* COD JA EXISTE,DADOS NAO GRAVADOS *"
				  TO MENS
                  PERFORM ROT-MENS
				  THRU ROT-MENS-FIM
                  GO TO ACE-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE DEPENDENTE" 
					  TO MENS
                      PERFORM ROT-MENS
					  THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
	            DISPLAY (20, 02) " N = NOVO REGISTRO".
				DISPLAY (21, 02) " A = ALTERAR REGISTRO".
				DISPLAY (22, 02) " E = EXCLUIR REGISTRO".
                DISPLAY (23, 02) " DIGITE SUA OPCAO: ".
                ACCEPT (23, 21) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3.
      *
       EXC-OPC.
                DISPLAY (22, 35) "      VOCE DESEJA EXCLUIR (S/N)?:".
                ACCEPT (22, 71) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADPAG RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO FUNCIONARIO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (20, 29) "ALTERAR  (S/N) : ".
                ACCEPT (20, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGPAG
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                   GO TO ROT-FIM.			
      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADPAG.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (20, 26) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (20, 26) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.