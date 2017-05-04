	IDENTIFICATION DIVISION.
	PROGRAM-ID. ALGO4.

	ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
	FILE-CONTROL.
		SELECT CONS1 ASSIGN TO DISK
					ORGANIZATION IS LINE SEQUENTIAL
					FILE STATUS IS FS-CONS1.
		SELECT CONS2 ASSIGN TO DISK
					ORGANIZATION IS LINE SEQUENTIAL
					FILE STATUS IS FS-CONS2.
		SELECT CONS3 ASSIGN TO DISK
					ORGANIZATION IS LINE SEQUENTIAL
					FILE STATUS IS FS-CONS3.
		SELECT CUENTAS ASSIGN TO DISK
					ORGANIZATION IS LINE SEQUENTIAL
					FILE STATUS IS FS-CTAS.
		SELECT ESTADOS ASSIGN TO DISK
					ORGANIZATION IS LINE SEQUENTIAL
					FILE STATUS IS FS-EST.
		SELECT MAESTRO ASSIGN TO DISK
					ORGANIZATION IS LINE SEQUENTIAL
					FILE STATUS IS FS-MAE.
		SELECT LISTADO ASSIGN TO DISK
					ORGANIZATION IS LINE SEQUENTIAL
					FILE STATUS IS FS-LIST.
	DATA DIVISION.
	FILE SECTION.
	FD CONS1 	LABEL RECORD IS STANDARD
				VALUE OF FILE-ID IS 'C:/cons1.dat'.
			 
	01 REG-CONS1.	
	   03 REG-CONS1-CUIT-CONS          PIC 9(15).
	   03 REG-CONS1-FECHA-ALTA         PIC X(10).
	   03 REG-CONS1-FECHA-BAJA         PIC X(10).
	   03 REG-CONS1-ESTADO             PIC 9(02).
	   03 REG-CONS1-NOMBRE-CONSORCIO   PIC X(30).
	   03 REG-CONS1-TEL                PIC X(15).
	   03 REG-CONS1-DIR                PIC X(30).

	FD CONS2 	LABEL RECORD IS STANDARD
				VALUE OF FILE-ID IS 'C:/cons2.dat'.
	01 REG-CONS2.	
	   03 REG-CONS2-CUIT-CONS          PIC 9(15).
	   03 REG-CONS2-FECHA-ALTA         PIC X(10).
	   03 REG-CONS2-FECHA-BAJA         PIC X(10).
	   03 REG-CONS2-ESTADO             PIC 9(02).
	   03 REG-CONS2-NOMBRE-CONSORCIO   PIC X(30).
	   03 REG-CONS2-TEL                PIC X(15).
	   03 REG-CONS2-DIR                PIC X(30).

	FD CONS3 	LABEL RECORD IS STANDARD
				VALUE OF FILE-ID IS 'C:/cons3.dat'.
	01 REG-CONS3.	
	   03 REG-CONS3-CUIT-CONS          PIC 9(15).
	   03 REG-CONS3-FECHA-ALTA         PIC X(10).
	   03 REG-CONS3-FECHA-BAJA         PIC X(10).
	   03 REG-CONS3-ESTADO             PIC 9(02).
	   03 REG-CONS3-NOMBRE-CONSORCIO   PIC X(30).
	   03 REG-CONS3-TEL                PIC X(15).
	   03 REG-CONS3-DIR                PIC X(30).

	FD CUENTAS LABEL RECORD IS STANDARD
	           VALUE OF FILE-ID IS "cuentas.dat".
	01 CTA. 
	   03 CTA-CUIT-CONS           PIC 9(15).
	   03 CTA-NRO-CTA             PIC 9(08).
	   03 CTA-FECHA-ALTA          PIC X(10).
	   03 CTA-ENTIDAD             PIC 9(03).
	   03 CTA-SUCURSAL            PIC 9(03).

	FD ESTADOS LABEL RECORD IS STANDARD
	           VALUE OF FILE-ID IS "estados.dat".   

	01 EST.
	   03 EST-ESTADO              PIC 9(02).
	   03 EST-DESCRIP             PIC X(15).

	FD MAESTRO LABEL RECORD IS STANDARD
	           VALUE OF FILE-ID IS "maestro.dat".

	01 MAE.
	   03 MAE-CUIT-CONS           PIC 9(15).
	   03 MAE-FECHA-ALTA          PIC X(10).
	   03 MAE-DESCRIP-ESTADO      PIC X(15).
	   03 MAE-NOMBRE-CONSORCIO    PIC X(30).
	   03 MAE-TEL                 PIC X(15).
	   03 MAE-DIR                 PIC X(30).
	   03 MAE-NRO-CTA             PIC 9(08).
	
	FD LISTADO LABEL RECORD OMITTED.
	01 LINEA                      PIC X(80).


		WORKING-STORAGE SECTION.
		77 FS-CONS1 		PIC XX.
		77 FS-CONS2 		PIC XX.
		77 FS-CONS3 		PIC XX.
		77 FS-CTAS			PIC XX.
		77 FS-EST			PIC XX.
		77 FS-MAE			PIC XX.
		77 FS-LIST			PIC XX.
		77 CUIT-N1          PIC 9(15).
		77 CUIT-N2          PIC 9(15).
		77 CUIT-N3          PIC 9(15).
		
		77 cantConsorcios 		PIC 99 VALUE 0.
		77 bajas 				PIC 99 VALUE 0.
		77 cantLineas 			PIC 99 VALUE 0.
		77 cantHojas 			PIC 99 VALUE 1.
		77 cantCons1 			PIC 99 VALUE 0.
		77 cantCons2 			PIC 99 VALUE 0.
		77 cantCons3 			PIC 99 VALUE 0.
		77 CANTESTADOS 			PIC 99 VALUE 0.
		77 CONT-ANIO 			PIC 99 VALUE 0.
	    77 WS-DESCRIP-ESTADO 	PIC X(15).
	    77 WS-CONT-ESTADOS		PIC 99.
	    01 CON-MENOR.
	    	03 CON-MENOR-CUIT-CONS          PIC 9(15).
	    	03 CON-MENOR-FECHA-ALTA         PIC X(10).
	    	03 CON-MENOR-FECHA-BAJA         PIC X(10).
	    	03 CON-MENOR-ESTADO             PIC 9(02).
	    	03 CON-MENOR-NOMBRE-CONSORCIO   PIC X(30).
	    	03 CON-MENOR-TEL                PIC X(15).
	    	03 CON-MENOR-DIR                PIC X(30).
	    
		 01 TAB-ESTADOS.
			03 TAB-ESTADOS-ELM 
					OCCURS 30 TIMES
					ascending key TAB-ESTADOS-ESTADO
					INDEXED BY IND.
				05 TAB-ESTADOS-ESTADO PIC X(02).
				05 TAB-ESTADOS-DESCRIP PIC X(15).
        01 FECHA.
           03 FECHA-AA    PIC 9(02).
           03 FECHA-MM    PIC 9(02).
           03 FECHA-DD    PIC 9(02).
      
        01 PE1-ENCABE.
           03 FILLER PIC X(07) VALUE 'Fecha: '.
           03 PE1-FECHA-DD PIC Z9.
           03 FILLER       PIC X VALUE '/'.
           03 PE1-FECHA-MM PIC 99.
           03 FILLER       PIC X VALUE '/'.
           03 PE1-FECHA-AA PIC 99.
           03 FILLER       PIC X(61) VALUE 
           '                                  Nro. Hoja: '.
           03 PE1-HOJA     PIC 9999 VALUE ZERO.
      
        01 PE2-ENCABE.
           03 FILLER PIC X(80) VALUE ALL ' '.
        
        01 PE3-ENCABE.
           03 FILLER   PIC X(25).
           03 FILLER   PIC X(30) VALUE 'LISTADO DE CONSORCIOS DE BAJA'.
           03 FILLER   PIC X(25).
      
        01 PB1-BAJA.
           03 FILLER   PIC X(13) VALUE 'CUIT-CONS'.
           03 FILLER   PIC X(13) VALUE 'FEC-ALTA'.
           03 FILLER   PIC X(13) VALUE 'FEC-BAJA'.
           03 FILLER   PIC X(13) VALUE 'NOMBRE'.
           03 FILLER   PIC X(13) VALUE 'TELEFONO'.
           03 FILLER   PIC X(13) VALUE 'DIRECCION'.
      
        01 PB2-BAJA.
           03 PB2-BAJA-CUIT-CONS   PIC 9(15).
           03 PB2-BAJA-FEC-ALTA    PIC X(10).
           03 PB2-BAJAR-FEC-BAJA   PIC X(10).
           03 PB2-BAJA-NOMBRE      PIC X(10).
           03 PB2-BAJA-TELEFONO    PIC X(15).
           03 PB2-BAJA-DIRECCION   PIC X(20).
      
        01 PB3-BAJA.
           03 F PIC X(26)
           		VALUE 'TOTAL NOVEDADES POR CUIT: '.
           03 PB3-TOTAL-NOV    PIC 9999 VALUE ZERO.
                
        01 PB-FINAL.
           03 F PIC X(35) 
           	  VALUE 'Total de Consorcios dados de baja: '.
           03 PB-FINAL-TOTAL PIC 9999 VALUE ZERO.
      
		PROCEDURE DIVISION.
			    perform INICIALIZAR.
          perform ABRIR-ARCHIVOS.
			    perform GEN-TABLA-ESTADOS.
			    perform LEO-CONSORCIO-1.
	        perform LEO-CONSORCIO-2.
	        perform LEO-CONSORCIO-3.
		    	perform LEO-CUENTAS.
			    perform IMPRIMO-ENCABEZADO.
			    perform CICLO-CONSORCIO.
			    perform IMPRIMO-BAJAS-FIN.
			    perform MOSTRAR-ESTADISTICAS.
			    perform CERRAR-ARCHIVOS.
		STOP RUN.
	  
       INICIALIZAR.
		        DISPLAY "INICIALIZAR INICIA".
                MOVE 0 TO cantCons1.
                MOVE 0 TO cantCons2.
                MOVE 0 TO cantCons3.
                MOVE 0 TO bajas.
                MOVE 1 TO cantHojas.
                MOVE 0 TO CONT-ANIO.
                ACCEPT FECHA FROM DATE.
                MOVE FECHA-AA TO PE1-FECHA-AA.
                MOVE FECHA-MM TO PE1-FECHA-MM.
                MOVE FECHA-DD TO PE1-FECHA-DD.
                DISPLAY "INICIALIZAR FIN".
      
		ABRIR-ARCHIVOS.
			DISPLAY "ABRIR-ARCHIVOS INICIA".
	        OPEN INPUT CONS1.
	        IF FS-CONS1 NOT = ZERO
	           DISPLAY "Err abrir Consorcios1: " FS-CONS1
	           STOP RUN.
	        OPEN INPUT CONS2.
	        IF FS-CONS2 NOT = ZERO
	           DISPLAY "Err abrir Consorcios2: " FS-CONS2
	           STOP RUN.
	        OPEN INPUT CONS3.
	        IF FS-CONS3 NOT = ZERO
	           DISPLAY "Err abrir Consorcios3: " FS-CONS3
	           STOP RUN.

	        OPEN INPUT CUENTAS.
	        IF FS-CTAS NOT = ZERO
	           DISPLAY "Error al abrir Cuentas: " FS-CTAS
	           STOP RUN.

	        OPEN INPUT ESTADOS.
	        IF FS-EST NOT = ZERO
	           DISPLAY "Error al abrir Estados: " FS-EST
	           STOP RUN.

	        OPEN OUTPUT MAESTRO.
	        OPEN OUTPUT LISTADO.
	        DISPLAY "ABRIR-ARCHIVOS FIN".

		GEN-TABLA-ESTADOS.
			DISPLAY "GEN-TABLA-ESTADOS".
			PERFORM LEO-ESTADO.
			PERFORM CARGAR-ESTADO VARYING WS-CONT-ESTADOS
				FROM 1 BY 1 
				UNTIL FS-EST = 10 OR WS-CONT-ESTADOS > 30.			
			PERFORM ORDENAR-TABLA-ESTADOS.	
		
		CARGAR-ESTADO.
	   		MOVE EST TO TAB-ESTADOS-ELM (WS-CONT-ESTADOS).
			PERFORM LEO-ESTADO.
			
		LEO-ESTADO.
			DISPLAY "LEO-ESTADO-INICIA".
			READ ESTADOS.
			IF FS-EST NOT = ZERO
				DISPLAY "Error al leer Estados: " FS-EST
				STOP RUN.			
			
		ORDENAR-TABLA-ESTADOS.
			DISPLAY "ORDENAR TABLA ESTADOS".
      
		LEO-CONSORCIO-1.
			DISPLAY "LEO-CONSORCIOS INICIA".
			READ CONS1.
			IF FS-CONS1 NOT = ZERO
				DISPLAY "Err leer consorcios1 " FS-CONS1
				STOP RUN.

	    LEO-CONSORCIO-2.
			DISPLAY "LEO-CONSORCIOS INICIA".
	        READ CONS2.
	        IF FS-CONS2 NOT = ZERO
	          DISPLAY "Err: leer consorcios2:" FS-CONS2
	          STOP RUN.
            
	    LEO-CONSORCIO-3.
	        DISPLAY "LEO-CONSORCIOS INICIA".
	        READ CONS3.
	        IF FS-CONS3 NOT = ZERO
	          DISPLAY "Err: leer consorcios3:" FS-CONS3
	          STOP RUN.    
  
	    LEO-CUENTAS.
			DISPLAY "LEO-CUENTAS".
	        READ CUENTAS.
	        IF FS-CTAS NOT = ZERO
	          DISPLAY "Error al leer cUENTas: " FS-CTAS
	          STOP RUN.

		OBTENER-ESTADO.
			DISPLAY "OBTENER ESTADO".
			SEARCH ALL TAB-ESTADOS-ELM
				AT END DISPLAY "Estado no encontrado"
			WHEN TAB-ESTADOS-ESTADO(IND) = CON-MENOR-ESTADO
				PERFORM OBTENER-INFO-ESTADO
			END-SEARCH.
				
	   OBTENER-INFO-ESTADO.
		DISPLAY "OBTENER INFORMACION DEL ESTADO".
		MOVE TAB-ESTADOS-DESCRIP(IND) TO WS-DESCRIP-ESTADO.
		
		IMPRIMO-ENCABEZADO.
                MOVE cantHojas TO PE1-HOJA.
		        WRITE LINEA FROM PE1-ENCABE.
                WRITE LINEA FROM PE2-ENCABE.
                WRITE LINRA FROM PE3-ENCABE.
                WRITE LINEA FROM PE2-ENCABE.
                ADD 1 TO cantHojas.
                MOVE 4 TO cantLineas.
       LISTAR-BAJA.
               IF cantLineas >= 60 
                  PERFORM IMPRIMIR_ENCABEZADO.
               PERFORM IMPRIMIR_BAJA.
               ADD 1 TO bajas.
        IMPRIMIR_BAJA-FIN.
               DISPLAY "IMPRIMIR-BAJA".
               MOVE bajas TP PF-FINAL-TOTAL.
               WRITE LINEA FROM PB-FINAL. 
	  IMPRIMIR-BAJA.
		            DISPLAY "IMPRIMO-BAJAS".
                WRITE LINEA FROM PB1-ENCABE.
                MOVE WS-CONS-MENOR-CUIT-CONS TO PB2-BAJA-CUIT-CONS.
                MOVE WS-CONS-MENOR-FECHA-ALTA TO PB2-BAJA-FEC-ALTA.
                MOVE WS-CONS-MENOR-FECHA-BAJA TO PB2-BAJAR-FEC-BAJA.
                MOVE WS-CONS-MENOR-NOMBRE-CONSORCIO TO PB2-BAJA-NOMBRE.
                MOVE WS-CONS-MENOR-TEL TO PB2-BAJA-TELEFONO.
                MOVE PB2-BAJA-DIRECCION TO PB2-BAJA-DIRECCION.
                WRITE LINEA FROM PB2-ENCABE.
                WRITE LINRA FROM PB3-ENCABE.
                WRITE LINEA FROM PE2-ENCABE.
                ADD 4 TO cantLineas.
		CICLO-CONSORCIO.
			DISPLAY "CICLO-CONSORCIO".
	        PERFORM DET-MENOR.
	        PERFORM POS-CUENTAS UNTIL FS-CTAS = '10' 
	                OR CTA-CUIT-CONS >= CON-MENOR-CUIT-CONS. 
	        PERFORM POS-CONSORN1 UNTIL FS-CONS1 = '10' 
	                OR REG-CONS1-CUIT-CONS IS NOT EQUAL 
	                TO CON-MENOR-CUIT-CONS. 
	        PERFORM POS-CONSORN2 UNTIL FS-CONS2 = '10' 
	                OR REG-CONS2-CUIT-CONS IS NOT EQUAL 
	                TO CON-MENOR-CUIT-CONS. 
	        PERFORM POS-CONSORN3 UNTIL FS-CONS3 = '10' 
	                OR REG-CONS3-CUIT-CONS IS NOT EQUAL 
	                TO CON-MENOR-CUIT-CONS. 
	        PERFORM OBTENER-ESTADO.
	        IF CON-MENOR-ESTADO = '02'
	           PERFORM LISTAR-BAJA
	        ELSE
	           PERFORM ALTA-MAESTRO.

		MOSTRAR-ESTADISTICAS.
			DISPLAY "MOSTRAR-ESTADISTICAS".

		CERRAR-ARCHIVOS.
			DISPLAY "CERRAR-ARCHIVOS".
			CLOSE CONS1.
			CLOSE CONS2.
			CLOSE CONS3.
			CLOSE CUENTAS.
			CLOSE ESTADOS.
			CLOSE MAESTRO.
			CLOSE LISTADO.

		 DET-MENOR.
                DISPLAY "DET-MENOR".
                MOVE REG-CONS1 TO WS-CONS-MENOR.
                IF REG-CONS2-CUIT-CONS < WS-CONS-MENOR-CUIT-CONS
                   MOVE REG-CONS2 TO WS-CONS-MENOR.
                IF REG-CONS3-CUIT-CONS < WS-CONS-MENOR-CUIT-CONS
                   MOVE REG-CONS3 TO WS-CONS-MENOR.
        POS-CUENTAS.
                DISPLAY "POS-CUENTAS".
                PERFORM  LEO-CUENTAS.
        POS-CONSORN1.
                PERFORM GENERAR-ESTADISTICAS.
                MOVE REG-CONS1 TO WS-CONS-MENOR.
                PERFORM LEO-CONSORCIO-1.
        POS-CONSORN2.
                PERFORM GENERAR-ESTADISTICAS.
                MOVE REG-CONS2 TO WS-CONS-MENOR.
                PERFORM LEO-CONSORCIO-2.
        POS-CONSORN3.
                PERFORM GENERAR-ESTADISTICAS.
                MOVE REG-CONS3 TO WS-CONS-MENOR.
                PERFORM LEO-CONSORCIO-3.
		
	    	ALTA-MAESTRO.
               MOVE CTA-NRO-CTA TO WS-NRO-CTA-AUX.
               IF WS-CONS-MENOR-CUIT-CONS NOT EQUAL 
                                   TO CTA-CUIT-CONS
                  MOVE 0 TO WS-NRO-CTA-AUX.
               ADD 1 TO cantConsorcios.
               MOVE WS-CONS-MENOR-FECHA-ALTA TO  MAE-FECHA-ALTA.
               MOVE WS-DESCRIP-ESTADO TO MAE-DESCRIP-ESTADO.
               MOVE WS-CONS-MENOR-NOMBRE-CONSORCIO 
                    TO MAE-NOMBRE-CONSORCIO.
               MOVE WS-CONS-MENOR-TEL TO MAE-TEL.
               MOVE WS-CONS-MENOR-DIR TO MAE-DIR.
               MOVE WS-NRO-CTA-AUX TO  MAE-NRO-CTA.
               WRITE MAE.

