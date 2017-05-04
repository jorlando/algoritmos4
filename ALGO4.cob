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
	    01 WS-CONS-MENOR.
	    	03 WS-CONS-MENOR-CUIT-CONS          PIC 9(15).
	    	03 WS-CONS-MENOR-FECHA-ALTA         PIC X(10).
	    	03 WS-CONS-MENOR-FECHA-BAJA         PIC X(10).
	    	03 WS-CONS-MENOR-ESTADO             PIC 9(02).
	    	03 WS-CONS-MENOR-NOMBRE-CONSORCIO   PIC X(30).
	    	03 WS-CONS-MENOR-TEL                PIC X(15).
	    	03 WS-CONS-MENOR-DIR                PIC X(30).
	    
		01 WS-TABLA-ESTADOS.
			03 WS-TABLA-ESTADOS-ELEMENTO OCCURS 30 TIMES 
				INDEXED BY IND.
				05 WS-TABLA-ESTADOS-ESTADO PIC 9(02).
				05 WS-TABLA-ESTADOS-DESCRIP PIC X(15).

		PROCEDURE DIVISION.
			perform ABRIR-ARCHIVOS.
			perform GEN-TABLA-ESTADOS.
			perform LEO-CONSORCIO-1.
	        perform LEO-CONSORCIO-2.
	        perform LEO-CONSORCIO-3.
			perform LEO-CUENTAS.
			perform IMPRIMO-ENCABEZADO.
			perform CICLO-CONSORCIO.
			perform IMPRIMO-BAJAS.
			perform MOSTRAR-ESTADISTICAS.
			perform CERRAR-ARCHIVOS.
		STOP RUN.
	      
		ABRIR-ARCHIVOS.
			DISPLAY "ABRIR-ARCHIVOS INICIA".
	        OPEN INPUT CONS1.
	        IF FS-CONS1 NOT = ZERO
	           DISPLAY "Error al abrir Consorcios 1: " FS-CONS1
	           STOP RUN.
	        OPEN INPUT CONS2.
	        IF FS-CONS2 NOT = ZERO
	           DISPLAY "Error al abrir Consorcios 2: " FS-CONS2
	           STOP RUN.
	        OPEN INPUT CONS3.
	        IF FS-CONS3 NOT = ZERO
	           DISPLAY "Error al abrir Consorcios 3: " FS-CONS3
	           STOP RUN.

	        OPEN INPUT CUENTAS.
	        IF FS-CTAs NOT = ZERO
	           DISPLAY "Error al abrir Cuentas: " FS-CTAs
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
					FROM 1 BY 1 UNTIL FS-EST = 10 OR WS-CONT-ESTADOS > 30.			
			PERFORM ORDENAR-TABLA-ESTADOS.	
			
		CARGAR-ESTADO.
			MOVE EST TO WS-TABLA-ESTADOS-ELEMENTO (WS_CONT_ESTADOS).
			PERFORM LEO-ESTADO.
			
		LEO-ESTADO.
			DISPLAY "LEO-ESTADO-INICIA".
			READ ESTADOS.
			IF FS-EST NOT = ZERO
				DISPLAY "Error al leer Archivo de Estados: " FS-EST.
				STOP RUN.			
			
		ORDENAR-TABLA-ESTADOS.
			DISPLAY "ORDENAR TABLA ESTADOS".
      
		LEO-CONSORCIO-1.
			DISPLAY "LEO-CONSORCIOS INICIA".
			READ CONS1.
			IF FS-CONS1 NOT = ZERO
				DISPLAY "Error al leer Consorcios 1: " FS-CONS1
				STOP RUN.
                
	    LEO-CONSORCIO-2.
			DISPLAY "LEO-CONSORCIOS INICIA".
	        READ CONS2.
	        IF FS-CONS2 NOT = ZERO
	          DISPLAY "Error al leer Consorcios 2: " FS-CONS2
	          STOP RUN.
            
	    LEO-CONSORCIO-3.
	        DISPLAY "LEO-CONSORCIOS INICIA".
	        READ CONS3.
	        IF FS-CONS3 NOT = ZERO
	          DISPLAY "Error al leer Consorcios 3: " FS-CONS3
	          STOP RUN.    
  
	    LEO-CUENTAS.
			DISPLAY "LEO-CUENTAS".
	        READ CUENTAS.
	        IF FS-CTAS NOT = ZERO
	          DISPLAY "Error al leer cUENTas: " FS-CTAS
	          STOP RUN.
           	
		OBTENER-ESTADO.
			DISPLAY "OBTENER ESTADO".
			SEARCH WS_TABLA_ESTADOS
				AT END DISPLAY "Estado no encontrado"
			WHEN WS_TABLA_ESTADOS_ESTADO(IND) = WS_CONS_MENOR_ESTADO
				PERFORM OBTENER-INFO-ESTADO.
			END SEARCH.
				
		OBTENER-INFO-ESTADO.
			DISPLAY "OBTENER INFORMACION DEL ESTADO".
			MOVE WS_TABLA_ESTADOS_DESCRIP(IND) TO WS_DESCRIP_ESTADO.	
		
		IMPRIMO-ENCABEZADO.
			DISPLAY "IMPRIMO-ENCABEZADO".
		IMPRIMO-BAJAS.
			DISPLAY "IMPRIMO-BAJAS".
		CICLO-CONSORCIO.
			DISPLAY "CICLO-CONSORCIO".
	        PERFORM DET-MENOR.
	        PERFORM POS-CUENTAS UNTIL FS-CTAS = '10' 
	                OR CTA-CUIT-CONS >= WS-CONS-MENOR-CUIT-CONS. 
	        PERFORM POS-CONSORN1 UNTIL FS-CONS1 = '10' 
	                OR REG-CONS1-CUIT-CONS IS NOT EQUAL 
	                TO WS-CONS-MENOR-CUIT-CONS. 
	        PERFORM POS-CONSORN2 UNTIL FS-CONS2 = '10' 
	                OR REG-CONS2-CUIT-CONS IS NOT EQUAL 
	                TO WS-CONS-MENOR-CUIT-CONS. 
	        PERFORM POS-CONSORN3 UNTIL FS-CONS3 = '10' 
	                OR REG-CONS3-CUIT-CONS IS NOT EQUAL 
	                TO WS-CONS-MENOR-CUIT-CONS. 
	        PERFORM OBTENER-ESTADO.
	        IF WS-CONS-MENOR-ESTADO = '02'
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


----------------------------------------
