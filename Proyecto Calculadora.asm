;INSTRUCCIONES DE USO:
;EN LOS PUERTOS DE ENTRADA 08H Y 09H SE DEBEN COLOCAR LOS DATOS A OPERAR
;EL PUERTO 0AH SE USARA PARA SELECCIONAR LA OPERACION A REALIZAR
;1. COLOCAR 01H EN EL PUERTO 01H PARA REALIZAR UNA SUMAR
;2. COLACAR 02H EN EL PUERTO 02H PARA REALIZAR UNA RESTAMOS
;3. COLOCAR 03H EN EL PUERTO 03H PARA REALIZAR UNA DIVISION (PARA ESTE ALGORITMO SE MOTRARA PRIMERO EL RESULTADO DEL COCIENTE Y LUEGO EL RESIDUO)
;4. COLOCAR CUALQUIER NUMERO O DEJAR EN 00H  EL PUERTO 03H PARA REALIZAR UNA MULTIPLICACION
;PROGRAMA REALIZADO POR:
;Diego Alejandro Restrepo Vela - 20171578080
;Eduard Daniel Lemos Arias -  20171578028
.ORG 0000H
  JMP INI

		D0 DB 06H, 09H, 09H, 09H, 09H, 06H
.ORG 0103H
		D1 DB 04H, 0CH, 04H, 04H, 04H, 0EH
.ORG 0203H
		D2 DB 06H, 09H, 02H, 04H, 08H, 0FH
.ORG 0303H
		D3 DB 0FH, 01H, 07H, 01H, 01H, 0FH
.ORG 0403H
		D4 DB 09H, 09H, 0FH, 01H, 01H, 01H
.ORG 0503H
		D5 DB 0FH, 08H, 0FH, 01H, 01H, 0FH
.ORG 0603H
		D6 DB 0FH, 08H, 0FH, 09H, 09H, 0FH
.ORG 0703H
		D7 DB 0FH, 09H, 01H, 01H, 01H, 01H
.ORG 0803H
		D8 DB 06H, 09H, 06H, 06H, 09H, 06H
.ORG 0903H
		D9 DB 0FH, 09H, 0FH, 01H, 01H, 01H
.ORG 0A00H

;PRIMERO INGRESAMOS LOS DOS NUMEROS Y LOS CONVERTINOS DE BCD A BINARIO
INI: MVI H, 10H                               ;INICIALIZAMOS H EN 10H, ESTE REGISTRO LO USAREMOS ASI A LOS LARGO DE TODO EL PROGRAMA
MVI L, 50H
;Entra el primer numero
IN 08H                                        ;ENTRA PRIMER NUMERO EN BCD
CPI 10H                                       ;COMPARAMOS SI LUEGO DE LA MASCARA EL RESULTADO NO ES IGUAL A CERO
JC NoHayConversion                            ;SI EL RESULTADO ES CERO ESTO SIGNIFICA QUE NO ES NECESARIO HACER UNA CONVERSION

MOV B, A                                      ;GUARDAMOS EL NUMERO EN EL REGISTRO B
ANI F0H													              ;hacemos una mascara para los bits de menor peso
RRC                                           ;DESPLAZAMOS EL NUMERO 4 VECES A LA DERECHA PARA OBTENER LAS DECENAS
RRC
RRC
RRC
MOV D, A                                      ;GUARDAMOS EL DATO A DECREMENTAR EN EL REGISTRO D
BCDaBinario: MOV A, C                         ;TRAEMOS EL DATO A SUMAR DEL REGISTRO C
  ADI 06H                                     ;LE SUMAMOS UN 06H
  MOV C, A                                    ;GUARDAMOS EL DATO EN C Nuevamente
  DCR D                                       ;DECREMENTAMOS EL REGISTRO D
  MOV A, D                                    ;MOVEMOS EL DATO AL Acumulador
  CPI 00H                                     ;LO COMPARAMOS CON 00H
JNZ BCDaBinario

MOV A, B                                      ;FINALMENTE RECUPERAMOS EL NUMERO ORIGINAL
SUB C                                         ;LE RESTAMOS LO NECESARIO PARA LA CONVERSION BCD/BCDaBinario

NoHayConversion: MOV M, A                     ;Y GUARDAMOS EL DATO CONVERTIDO EN LA POSICION DE MEMOARIA 1050H

;Entra el segundo numero
IN 09H                                        ;ENTRA SEGUNDO NUMERO EN BCD
CPI 10H                                       ;COMPARAMOS SI LUEGO DE LA MASCARA EL RESULTADO NO ES IGUAL A CERO
JC NoHayConversion2                           ;SI EL RESULTADO ES CERO ESTO SIGNIFICA QUE NO ES NECESARIO HACER UNA CONVERSION
MOV B, A                                      ;GUARDAMOS EL NUMERO EN EL REGISTRO B
ANI F0H												                ;hacemos una mascara para los bits de menor peso
RRC                                           ;DESPLAZAMOS EL NUMERO 4 VECES A LA DERECHA PARA OBTENER LAS DECENAS
RRC
RRC
RRC
MOV D, A                                      ;GUARDAMOS EL DATO A DECREMENTAR EN EL REGISTRO D
MVI C, 00H                                    ;INICIAMOS C EN 00H POR SI QUEDO ALGUN NUMERO DE LA CONVERSION ANTERIOR
BCDaBinario2: MOV A, C                        ;TRAEMOS EL DATO A SUMAR DEL REGISTRO C
  ADI 06H                                     ;LE SUMAMOS UN 06H
  MOV C, A                                    ;GUARDAMOS EL DATO EN C Nuevamente
  DCR D                                       ;DECREMENTAMOS EL REGISTRO D
  MOV A, D                                    ;MOVEMOS EL DATO AL Acumulador
  CPI 00H                                     ;LO COMPARAMOS CON 00H
JNZ BCDaBinario2

MOV A, B                                      ;FINALMENTE RECUPERAMOS EL NUMERO ORIGINAL
SUB C                                         ;LE RESTAMOS LO NECESARIO PARA LA CONVERSION BCD/BCDaBinario

NoHayConversion2: INR L
MOV M, A                                      ;Y GUARDAMOS EL DATO CONVERTIDO EN LA POSICION DE MEMOARIA 1051H
;AQUI TERMINAMOS LA CONVERSION DE BCD A BINARIO


;INGRESAMOS EL NUMERO QUE IDENTIFICARA EL TIPO DE OPERACION POR EL PUERTO 0AH
  IN 0AH                                      ;INGRESAMOS EL DATO DE LA OPERACION POR EL PUERTO 02H
  CPI 01H                                     ;SI EN EL PUERTO 02H HAY UN 01H, SUMAMOS
  JNZ SUMA                                    ;SALTAMOS SI NO HAY UN 01H

;ESCRIBIMOS AQUI ADENTRO EL CODIGO DE LA SUMA

    MVI L, 50H                                ;UBICAMOS EL PRIMER NUMEROS
    MOV A, M                                  ;TRAEMOS EL NUMERO AL Acumulador
    INR L                                     ;AUMENTAMOS LA MEMORIA PARA LLEGAR AL SEGUNDO NUMEROS
    ADD M                                     ;SUMAMOS LOS DOS numeros

    MVI L, 50H                                ;DIRECCIONAMOS A LA POSICION DE MEMORIA DEL PRIMER NUMERO
    MOV M, A                                  ;AQUI GUARDAMOS EL RESULTADO

  ;CONVERTIMOS EL RESULTADO DE LA SUMA DE BINARIO A BCD
    CPI 64H                                   ;COMPARAMOS PARA VERIFICAR SI ES MENOR A 100
    JC SUMAMENOR100                           ;SALTAMOS SI ES MENOR A 100
    ;AQUI OPERAREMOS UN RESULTADO DE SUMA MAYOR A 100

    MOV B,M			                              ;Guardamos el dato en el registro B
    MVI D,6EH                                 ;Inicializamos el registro D con 6EH (110) para empezar a comparar desde aqui
    MVI C,9CH

    CILO100ESPECIAL:MOV A,B		                ;Recuperamos el dato de B en el acumulador (necesario para lo siguientes pasos)
      CMP D                                   ;Comparamos con D, si es menor saltamos al final y sumamos C que
      JC C100E				                        ;idealmente C debe valer cero para este caso

        MOV A,C	                              ;Movemos el contenido de C al acumulador
        ADI 06H                               ;Sumamos 06H a este contenido
        MOV C,A                               ;Guardamos el resultado en C nuevamente
        MOV A,D                               ;recuperamos el valor de D
        ADI 0AH                               ;y le sumamos 0AH para la siguiente iteracion
        MOV D,A                               ;Nuevamente guardamos el valor
      JMP CILO100ESPECIAL                     ;Nos de volvemos a C1
    C100E: MOV A, C                           ;OBTENEMOS EL DATO A Sumar
    ADD M                                     ;SE LO SUMAMOS AL NEABLE BAJO 1050H
    MOV M, A                                  ;GUARDAMOS EL DATO EN 1050H
    INR L                                     ;INCREMENTAMOS LA MEMORIA PARA ACCEDER AL NEABLE ALTO
    MVI M, 01H                                ;Le dejamos un 1 al neable alto
    MVI L, 51H                                ;APUNTAMOS AL NEABLE ALTO

    ;MOSTRAMOS EL RESULTADO DE 3 DIGITOS DE LA SUMA
     MOV A,M                                  ;Lo pasamos al acumulador
    	ANI 0FH                                 ;le Hacemos una mascara
     CALL VISUALIZACION                       ;Llamamos a la funcion para rotar

     ;ROTAMOS EL NEABLE BAJO
     MVI H, 10H
     MVI L, 50H
     MOV A,M			 										        ;TRAEMOS EL DATO DEL NEABLE ALTO AL ACUMULADOR(YA QUE PRIMERO DEBEMOS ROTAR EL NEABLE ALTO)
    	ANI F0H													        ;hacemos una mascara para los bits de menor peso
      RRC 							                      ;ROTAMOS 4 VECES A LA DERECHA SIN ACARREO
      RRC                                     ;ROTAMOS HACIA LA DERECHA PARA PONER EL DATO EN
      RRC 																    ;ENABLE BAJO
    	RRC
     CALL VISUALIZACION                       ;Llamamos a la funcion para rotar
      MVI H, 10H
    	MVI L, 50H                              ;RECUPERAMOS EL DATO PARA ROTAR Y MOSTRAR SUS PRIMEROS 4 BITS
      MOV A,M                                 ;Lo pasamos al acumulador
    	ANI 0FH                                 ;le Hacemos una mascara
     CALL VISUALIZACION                       ;Llamamos a la funcion para rotar
    HLT                                       ;TERMINAMOS LA EJECUCION

  SUMAMENOR100: CALL MOSTRARMENORA100         ;CON ESTA FUNCION MOSTRAMOS LOS NUMEROS MENORES A 100

  SUMA:CPI 02H                                ;SI EN EL PUERTO 0AH HAY UN 02H, RESTAMOS
  JNZ RESTA                                   ;SALTAMOS SI NO HAY UN 02H

  ;ESCRIBIR AQUI CODIGO DE RESTA
  MVI L, 50H;<-- puede que sobre
  MOV A, M                                    ;Traemos el minuendo al acumulador
  INR	L                                       ;incrementamos para llegar al sustraendo
  SUB	M                                       ;restamos el sustraendo al minuendo

  JNC POSITIVO                                ;SALTA SI EL RESULTADO ES POSITIVO
    MVI A, 7EH                                ;INGRESAMOS UN 7E POR EL PUERTO 03 PARA REPRESNETAR UN MENOS
    OUT 03H                                   ;MOSTRAMOS EL MENOS

    MVI L, 51H                                ;si el resultado es negativo, hacemos la resta inversa
    MOV A, M                                  ;TRAEMOS EL SUSTRAENDO COMO MINUENDO
    MVI L, 50H                                ;APUNTAMOS AL MINUENDO QUE AHORA SERA SUSTRAENDO
    SUB	M                                     ;RESTAMOS
    MOV M, A                                  ;GUARDAMOS EL RESULTADO

    CALL MOSTRARMENORA100                     ;SI ES NEGATIVO MOSTRAMOS CON UN MENOS
  POSITIVO: MVI L, 50H                        ;APUNTAMOS A LA MEMORIA PARA GUARDAR EL RESULTADO
  MOV M, A                                    ;GUARDAMOS EL RESULTADO
  CALL MOSTRARMENORA100                       ;MOSTRAMOS EL RESULTADO NORMAL SI ES POSITIVO


  RESTA: CPI 03H                              ;SI EN EL PUERTO 0AH HAY UN 03H, DIVIDIMOS
  JNZ DIVISION                                ;SALTAMOS SI NO HAY UN 03H

  ;ESCRIBIT AQUI CODIGO DE LA DIVISION

    MVI L, 50H                                ;UBICAMOS EL DIVIDENDO
    MOV A, M                                  ;GUARDAMOS EL DIVIDENDO EN EL ACUMULADOR
    INR L                                     ;INCREMENTAMOS PARA LLEGAR AL DIVISOR
    CMP M                                     ;COMPARAMOS EL DIVIDENDO CON EL DIVISOR
    JC DiviMenor                              ;SI EL DIVISOR ES MAYOR AL DIVIDENDO, SALTAMOS
    JZ DiviEqual                              ;SI SON IGUALES EL COCIENTE Y EL DIVISOR, SALTAMOS

        ;SI TODO SALE BIEN PROCEDEMOS CON EL PROCESO NORMAL DE DIVISION
        MVI L, 50H
        MOV B, M                              ;TRAEMOS A B, EL DIVIDENDO
        MVI D, 00H
        MVI L, 51H                            ;APUNTAMOS AL DIVISOR

        ResiEqual: ANA A
          ResiMayor: MOV A, M                 ;TRAEMOS EL DIVISOR AL ACUMULADOR
            CMA                               ;COMPLEMENTAMOS AL DIVISOR
            ADD B                             ;SUMAMOS EL RESIDUO CON EL DIVISOR COMPLEMENTADO
            ACI 00H                           ;SUMAMOS EL ACARREO SI LO HAY

            MVI L, 50H
            CMP M
            JC UNO
              ADI 01H
            UNO:MVI L, 51H
            MOV B, A                          ;GUARDAMOS EL RESIDUO EN B
            INR D                             ;AUMENTAMOS EL COCIENTE QUE ESTARA EN D
            CMP M                             ;APROVECHAMOS QUE EL RESIDUO SIGUE ESTANDO EN EL ACUMULADOR PARA HACER LA COMPARACIÓN
          JNC ResiMayor
        JZ ResiEqual                          ;SI EL RESIDUO ES IGUAL AL DIVISOR HACEMOS UNA ITERACION MAS

        ;RESIDUO QUEDA EN B Y CONCIENTE QUEDA EN D
        MVI L, 50H
        MOV M, D
        INR L
        MOV M, B

        MVI C, 00H
        MVI L, 50H
        MOV A, M
        CPI 0AH                                 ;VERIFICAMOS SI EL DIVIDENDO ES MENOR A 10
        JC COCIENTEPEQUENO                      ;VERIFICAMOS QUE EL COCIENTE SEA MENOR A 0AH, SI LO ES SALTA
          CALL CICLO10SPECIAL
          MOV A, C                              ;OBTENEMOS EL DATO A Sumar
          ADD M                                 ;SE LO SUMAMOS AL NEABLE BAJO 1050H
          MOV M, A                              ;GUARDAMOS EL DATO EN 1050H
          MVI L, 50H                            ;UBICAMOS EL COCIENTE

          ;LLAMAMOS A LAS ROTACIONES DE 3 NUMEROS
          ;ROTAMOS LOS DOS NUMEROS DEL COCIENTE
          MOV A,M			 										      ;TRAEMOS EL DATO DEL NEABLE ALTO AL ACUMULADOR(YA QUE PRIMERO DEBEMOS ROTAR EL NEABLE ALTO)
         	ANI F0H													      ;hacemos una mascara para los bits de menor peso
          RRC 							                    ;ROTAMOS 4 VECES A LA DERECHA SIN ACARREO
          RRC                                   ;ROTAMOS HACIA LA DERECHA PARA PONER EL DATO EN
          RRC 																  ;ENABLE BAJO
         	RRC
          CALL VISUALIZACION                    ;Llamamos a la funcion para rotar
           MVI H, 10H
         	 MVI L, 50H                           ;RECUPERAMOS EL DATO PARA ROTAR Y MOSTRAR SUS PRIMEROS 4 BITS
           MOV A,M                              ;Lo pasamos al acumulador
         	ANI 0FH                               ;le Hacemos una mascara
          CALL VISUALIZACION                    ;Llamamos a la funcion para rotar

          MVI A, 03H                            ;PONEMOS UN SIMBOLO PARA INDICAR QUE VIENE EL RESIDUO
          OUT 00H                               ;MOSTRAMOS EL SIMBOLO

          ;ROTAMOS EL RESIDUO
          MVI H, 10H
          MVI L, 51H                            ;RECUPERAMOS EL DATO PARA ROTAR Y MOSTRAR SUS PRIMEROS 4 BITS
          MOV A,M                               ;Lo pasamos al acumulador
          ANI 0FH                               ;le Hacemos una mascara
          CALL VISUALIZACION                    ;Llamamos a la funcion para rotar

          HLT                                   ;INTERRUMPIMOS EL PROGRAMA

        COCIENTEPEQUENO:MOV C, A                ;INVERTIMOS LAS POSICIONES DEL COCIENTE Y EL RESIDUO
          INR L                                   ;INCREMENTAMOS PARA EL RESIDUO
          MOV A, M                                ;GUARDAMOS EL RESIDUO
          MOV M, C                                ;GUARDAMOS EL COCIENTE EL LA POSICION DEL RESIDUO
          MVI L, 50H                              ;MOVEMOS L A LA POSICION DEL COCIENTE
          MOV M, A                                ;Y GUARDAMOS EL RESIDUO EN EL COCIENTE
          MVI C, 00H                              ;INICIALIZO C EN 00H
          CALL CICLO10SPECIAL
          MOV A, C                              ;OBTENEMOS EL DATO A Sumar
          ADD M                                 ;SE LO SUMAMOS AL NEABLE BAJO 1050H
          MOV M, A                              ;GUARDAMOS EL DATO EN 1050H

          ;LLAMAMOS A LAS ROTACIONES DE 3 NUMEROS
          ;ROTAMOS EL UNICO NUMERO DEL COCIENTE
          MVI L, 51H                            ;RECUPERAMOS EL DATO PARA ROTAR Y MOSTRAR SUS PRIMEROS 4 BITS
          MOV A,M                               ;Lo pasamos al acumulador
          ANI 0FH                               ;le Hacemos una mascara
          CALL VISUALIZACION                    ;Llamamos a la funcion para rotar

          MVI A, 03H                            ;PONEMOS UN SIMBOLO PARA INDICAR QUE VIENE EL RESIDUO
          OUT 00H                               ;MOSTRAMOS EL SIMBOLO

          ;ROTAMOS LOS DOS NUMEROS DEL RESIDUO
          MVI H, 10H
          MVI L, 50H                            ;UBICAMOS EL RESIDUO
          MOV A,M			 										      ;TRAEMOS EL DATO DEL NEABLE ALTO AL ACUMULADOR(YA QUE PRIMERO DEBEMOS ROTAR EL NEABLE ALTO)
          ANI F0H													      ;hacemos una mascara para los bits de menor peso
          RRC 							                    ;ROTAMOS 4 VECES A LA DERECHA SIN ACARREO
          RRC                                   ;ROTAMOS HACIA LA DERECHA PARA PONER EL DATO EN
          RRC 																  ;ENABLE BAJO
          RRC
          CALL VISUALIZACION                    ;Llamamos a la funcion para rotar
           MVI H, 10H
           MVI L, 50H                           ;RECUPERAMOS EL DATO PARA ROTAR Y MOSTRAR SUS PRIMEROS 4 BITS
           MOV A,M                              ;Lo pasamos al acumulador
          ANI 0FH                               ;le Hacemos una mascara
          CALL VISUALIZACION                    ;Llamamos a la funcion para rotar


          HLT                                   ;INTERRUMPIMOS EL CICLO

    ;Cuando el divisor es mayor que el dividendo, el residuo será el dividendo y el cociente cero
    DiviMenor: MVI L, 51H
      MVI M, 00H                              ;ENVIAMOS UN CERO PARA EL COCIENTE, QUE PARA ESTE CASO LO TRABAJAREMOS CON LA POSICION 50H EN L
      MVI C, 00H                              ;INICIALIZAMOS C EN 00H

      MVI L, 50H
      MOV A, M
      CPI 0AH                                 ;VERIFICAMOS SI EL DIVIDENDO ES MENOR A 10
      JC DIVIDENDOPEQUENO
            CALL CICLO10SPECIAL
              MOV A, C                              ;OBTENEMOS EL DATO A Sumar
              ADD M                                 ;SE LO SUMAMOS AL NEABLE BAJO 1050H
              MOV M, A                              ;GUARDAMOS EL DATO EN 1050H

              ;LLAMAMOS A LAS ROTACIONES DE 3 NUMEROS
              MVI L, 51H                            ;POSICIONAMOS EL COCIENTE QUE SERA CERO

              ;ROTAMOS EL COCIENTE
              MOV A,M                               ;Lo pasamos al acumulador
              CALL VISUALIZACION                    ;Llamamos a la funcion para rotar

              MVI A, 03H                            ;PONEMOS UN SIMBOLO PARA INDICAR QUE VIENE EL RESIDUI
              OUT 00H                               ;MOSTRAMOS EL SIMBOLO

              ;ROTAMOS EL RESIDUO
              MVI H, 10H
              MVI L, 50H
              MOV A,M			 										      ;TRAEMOS EL DATO DEL NEABLE ALTO AL ACUMULADOR(YA QUE PRIMERO DEBEMOS ROTAR EL NEABLE ALTO)
             	ANI F0H													      ;hacemos una mascara para los bits de menor peso
              RRC 							                    ;ROTAMOS 4 VECES A LA DERECHA SIN ACARREO
              RRC                                   ;ROTAMOS HACIA LA DERECHA PARA PONER EL DATO EN
              RRC 																  ;ENABLE BAJO
             	RRC
              CALL VISUALIZACION                    ;Llamamos a la funcion para rotar
               MVI H, 10H
             	MVI L, 50H                            ;RECUPERAMOS EL DATO PARA ROTAR Y MOSTRAR SUS PRIMEROS 4 BITS
               MOV A,M                              ;Lo pasamos al acumulador
             	ANI 0FH                               ;le Hacemos una mascara
              CALL VISUALIZACION                    ;Llamamos a la funcion para rotar



            HLT                                     ;INTERRUMPIMOS EL CICLO PARA TERMINAR

      ;LLAMAMOS A LAS ROTACIONES DE DOS NUMEROS
      DIVIDENDOPEQUENO: MVI L, 51H            ;POSICIONAMOS EL COCIENTE QUE SERA CERO

      ;ROTAMOS EL COCIENTE
      MOV A, M                              ;Lo pasamos al acumulador
      CALL VISUALIZACION                    ;Llamamos a la funcion para rotar

      MVI A, 03H                            ;PONEMOS UN SIMBOLO PARA INDICAR QUE VIENE EL RESIDUI
      OUT 00H                               ;MOSTRAMOS EL SIMBOLO

      ;ROTAMOS EL RESIDUO
      MVI H, 10H
      MVI L, 50H                            ;RECUPERAMOS EL DATO PARA ROTAR Y MOSTRAR SUS PRIMEROS 4 BITS
      MOV A,M                               ;Lo pasamos al acumulador
      ANI 0FH                               ;le Hacemos una mascara
      CALL VISUALIZACION                    ;Llamamos a la funcion para rotar

      HLT                                   ;INTERRUMPIMOS EL PROGRAMA

    ;CUANDO EL DIVISOR Y EL COCIENTE SON IGUALES EL COCIENTE SERA 1 Y EL RESIDUO CERO
    DiviEqual: MVI L, 50H                ;ENVIAMOS UN CERO AL RESIDUO

      MVI M, 10H                         ;CON EN ENVIAR UN 10 NOS AHORRAMOS LA CONVERSION Y UBICAR LOS DATOS EN LA MEMORIA

      MOV A,M			 										   ;TRAEMOS EL DATO DEL NEABLE ALTO AL ACUMULADOR(YA QUE PRIMERO DEBEMOS ROTAR EL NEABLE ALTO)
     	ANI F0H													   ;hacemos una mascara para los bits de menor peso
      RRC 							                 ;ROTAMOS 4 VECES A LA DERECHA SIN ACARREO
      RRC                                ;ROTAMOS HACIA LA DERECHA PARA PONER EL DATO EN
      RRC 															 ;ENABLE BAJO
     	RRC
      CALL VISUALIZACION                 ;Llamamos a la funcion para rotar

      MVI A, 03H                         ;ESTE SERA EL SIMBOLO QUE INDIQUE QUE VIENE EL RESIDUO
      OUT 00H                            ;IMPRIMIMOS POR EL PUERTO 03H

       MVI H, 10H
     	MVI L, 50H                         ;RECUPERAMOS EL DATO PARA ROTAR Y MOSTRAR SUS PRIMEROS 4 BITS
       MOV A,M                           ;Lo pasamos al acumulador
     	ANI 0FH                            ;le Hacemos una mascara
      CALL VISUALIZACION                 ;Llamamos a la funcion para rotar


  HLT

  DIVISION: ANA A                             ;MULTIPLICAMOS SI HAY CUALQUIER OTRO NUMERO EN EL PUERTO 02H, MULTIPLICAMOS

  ;ESCRIBIR AQUI CODIGO DE LA MULTIPLIACION
  MVI L, 50H                                ;DIRECCIONAMOS AL PRIMER DATO QUE SERA EL MULTIPLICANDO
  MOV C, M                             	 	  ;guardamos el MULTIPLICANDO en el registro c
  MVI M, 00H                                ;PONEMOS UN 00H EN LA MEMORIA
  INR L                               	 	  ;DIRECCIONAMOS AL SEGUNDO DATO QUE SERA EL MULTIPLICADOR
  MOV D, M                             	 	  ;GUARDAMOS EL MULTIPLICADOR EN EL REGISTRO D
  MVI M, 00H                                ;PONEMOS UN 00H EN LA MEMORIA

  MVI B, 08H       	 	                      ;GUARDAMOS UN FFH EN EL REGISTRO B, ESTE NUMERO REPRESENTA LOS BITS A ROTAR

  BitsARotar: MVI L,51H

     MOV A,M 	                              ;MOVEMOS EL NEABLE ALTO PARA ROTARLO
      ANA A         	                      ;DEJAMOS EL ACARREO EN 0
     RAL            	                      ;ROTAMOS A LA IZQUIERDA CON ACARREO
      MOV M,A       	                      ;GUARDAMOS EL RESULTADO ROTADO EN E

      DCX H         	                      ;DECREMENTAMOS &HL PARA OBTENER SU NEABLE BAJO
      MOV A,M 		  	                      ;MOVEMOS EL NEABLE ALTO PARA ROTARLO
      ANA A     	                          ;DEJAMOS EL ACARREO EN 0
      RAL        	                          ;ROTAMOS A LA IZQUIERDA CON ACARREO
      MOV M,A     	                        ;GUARDAMOS EL RESULTADO ROTADO EN E

   JNC AcaNeable                         	    ;COMPROBAMOS SI HUBO ACARREO DEL NEABLE BAJO PARA SUMARLO AL NEABLE ALTO
      INX H         	                        ;INCREMENTAMOS &HL PARA USAR EL NEABLE ALTO
      MOV A,M       	                        ;TRAEMOS EL CONTENIDO DIRECCIONADO POR &HL AL Acumulador
      ACI 00H                             	  ;SUMAMOS 00H + EL CONTENIDO DEL ACUMULADOR + EL ACARREO QUE DEJO EL NEABLE BAJO (SI LO HAY)
      MOV M, A      		                      ;GUARDAMOS EL RESULTADO EN EN EL NEABLE ALTO 1051H
      DCX H         	                    	  ;VOLVEMOS AL NEABLE BAJO
   AcaNeable:MOV A,D	                    	  ;TRAEMOS EL MULTIPLICADOR A "A"
     RAL                                    ;ROTAMOS EL MULTIPLICADOR
     MOV D,A                                 ;GUARDAMOS EL DATO ROTADO EN D

    JNC VerifAca                        			  ;SI HAY ACARREO AL ROTAR EL MULTIPLICADOR, SUMAMOS EL MULTIPLICANDO
     ANA A                              		  ;COLOCAMOS EL ACARREO EN CERO DENUEVO
     MOV A,C                             	    ;SI HAY ACARREO PONEMOS EL MULTIPLICANDO EN EL ACUMULADOR
     ADD M					                    		  ;Y LO SUMAMOS CON EL RESULTADO QUE ESTAMOS GUARDANDO EN E
     MOV M,A                                  ;EL RESUSLTADO DE ESTO LO GUARDAMOS EN EL NEABLE BAJO

     INX H                                    ;INCREMENTAMOS &HL PARA USAR EL NEABLE ALTO
       MOV A,M                                ;TRAEMOS EL CONTENIDO DIRECCIONADO POR &HL AL Acumulador
       ACI 00H                                ;SUMAMOS 00H + EL CONTENIDO DEL ACUMULADOR + EL ACARREO QUE DEJO EL NEABLE BAJO (SI LO HAY)
       MOV M,A                                ;GUARDAMOS EL RESULTADO EN EN EL NEABLE ALTO 1051H
    VerifAca:DCR B                             ;SE DECREMENTA B

 JNZ BitsARotar                              ;COMPARAMOS SI B LLEGO A CERO PARA TERMINAR LOS SALOT

  CALL CONVERSOR1000
;>>>>>>>>>>>>>>>>>>>AQUI TERMINAN LOS LLAMADOS PRINCIPALES<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<










;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>AQUI IRAN LAS FUNCIONES<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;>>>>>>>CICLO DE 1000 EN 1000<<<<<<<<<<
CICLO1000: ANA A                    ;COLOCAMOS EL ACARREO EN CERO

  ;SUMAMOS EL NEABLE BAJO DEL NUMERO A COMPARAR
  C1000:MVI E, 58H        ;HUBICAMOS EL NEABLE BAJO DEL NUMERO A COMPARAR
    ANA A                 ;<---- POR SI ACASO
    LDAX D                ;TRAEMOS EL CONTENIDO DEL NEABLE BAJO DE B PARA SUMARLO CON
    ADI E8H               ;SUMAMOS EL 18H DEL NEABLE BAJO DE 3096
    STAX D                ;ENVIAMOS EL NUMERO SUMADO AL NEABLE BAJO

    ;SUMAMOS EL ACARREO QUE DEJO EL NEABLE BAJO AL NEABLE ALTO
    INR E    ;!           ;INCREMENTAMOS &DE PARA USAR EL NEABLE ALTO
    LDAX D                ;TRAEMOS EL CONTENIDO DEL NEABLE ALTO PARA SUMARLO CON EL ACARREO
    ACI 00H               ;SUMAMOS 00H + EL CONTENIDO DEL ACUMULADOR + EL ACARREO QUE DEJO EL NEABLE BAJO (SI LO HAY)

    ;SUMAMOS EL NEABLO ALTO DEL NUMERO A COMPARAR
    ADI 03H               ;LE SUMAMOS EL DATO AL NEABLE ALTO
    STAX D                ;ENVIAMOS EL NUMERO SUMADO + EL ACARREO DEL NEABLE BAJO (SI LO HUBO) AL NEABLE ALTO

    ANA A
    ;Hacemos la comparacion
    MVI L, 51H         ;TRAEMOS EL NEABLE ALTO DEL NUMERO MULTIPLICADO PARA COMPARARLOS PRIMERO
    XCHG                    ;CAMBIE  DE CON HL
    LDAX D                  ;TRAEMOS EL NEABLE ALTO DEL NUMERO A COMPARAR PARA COMPARLO CON EL NEABLE ALTO DEL DATO ORIGINAL
    CMP M                   ;COMPARAMOS SI EL NEABLE A COMPARAR (03) ES MENOR AL NEABLE ALTO DEL DATO MULTIPLICADO (A<M -> C=1 Y SALTA)

    JC NeablePequeÃ±o         ;SI EL NUMERO ES MENOR A 1000 ACABAMOS LA FUNCION, SINO ENTRAMOS A SUMAR DE 1000 EN 1000

      ;Guardamos el numero en la posicion que necesitamos
      MVI C, 58H
      MVI E, 52H
      LDAX B
      STAX D
      INR C
      INR E
      LDAX B
      STAX D


      MVI C, 54H             ;INICIAMOS &BC EN 1054, AQUI Y EN 1055 GUARDAREMOS EL NUMERO A SUMAR
      ;SUMAMOS EL NEABLE BAJO DEL NUMERO A SUMAR
      ANA A ;<---- POR SI ACASO
      LDAX B                ;TRAEMOS EL CONTENIDO DEL NEABLE BAJO DE B PARA SUMARLO CON
      ADI 18H               ;SUMAMOS EL 18H DEL NEABLE BAJO DE 3096
      STAX B                ;ENVIAMOS EL NUMERO SUMADO AL NEABLE BAJO

      ;SUMAMOS EL ACARREO QUE DEJO EL NEABLE BAJO AL NEABLE ALTO
      INR C    ;!           ;INCREMENTAMOS &DE PARA USAR EL NEABLE ALTO
      LDAX B                ;TRAEMOS EL CONTENIDO DEL NEABLE ALTO PARA SUMARLO CON EL ACARREO
      ACI 00H               ;SUMAMOS 00H + EL CONTENIDO DEL ACUMULADOR + EL ACARREO QUE DEJO EL NEABLE BAJO (SI LO HAY)


      ;SUMAMOS EL NEABLO ALTO DEL NUMERO A SUMAR
      ADI 0CH               ;LE SUMAMOS EL DATO AL NEABLE ALTO
      STAX B                ;ENVIAMOS EL NUMERO SUMADO + EL ACARREO DEL NEABLE BAJO (SI LO HUBO) AL NEABLE ALTO

  JMP C1000          ;Nos de volvemos a C1
NeablePequeÃ±o: RET
;>>>>>>>FIN CICLO DE 1000 EN 1000<<<<<<<<<<



;>>>>>CICLO DE 100 EN 100<<<<<<<<<<
CICLO100Y10: ANA A             ;POR SI YA HAY ACARREO

  C100: MVI E, 58H          ;INICIAMOS &DE EN 1052, AQUI GUARDAREMOS EL NUMERO A COMPARAR
    LDAX D                  ;TRAEMOS EL CONTENIDO DEL NEABLE BAJO DEL NUMERO A COMPARAR PARA SUMARLE 100
    MVI L, 60H
    ADD M                   ;SUMAMOS 100(64H) AL NEABLE BAJO
    STAX D                  ;INGRESAMOS EL RESULTADO AL NEABLE BAJO DEL NUMERO A COPARAR

    ;POR SI QUEDA ACARREO CON EL NUMERO A COMPARAR
    INR E                   ;INCREMENTAMOS &DE PARA USAR EL NEABLE ALTO DEL NUMERO A COMPARAR
    LDAX D                  ;TRAEMOS EL CONTENIDO DEL NEABLE ALTO DEL NUMERO A COMPARAR AL ACUMULADOR PARA SUMARLO CON EL ACARREO
    ACI 00H                 ;SUMAMOS 00H + EL CONTENIDO DEL ACUMULADOR + EL ACARREO QUE DEJO EL NEABLE BAJO (SI LO HAY)
    STAX D                  ;INGRESAMOS EL RESULTADO AL NEABLE BAJO DEL NUMERO A COPARAR

    ANA A                    ;COLOCAMOS EL ACARREO EN CERO

    ;COMPARAR LOS NEABLES ALTOS PARA VER CUAL ES MAYOR
    MVI L, 51H          ;TRAEMOS EL NEABLE ALTO DEL NUMERO MULTIPLICADO PARA COMPARARLOS PRIMERO
    XCHG                    ;CAMBIE  DE CON HL
    LDAX D                  ;TRAEMOS EL NEABLE ALTO DEL NUMERO A COMPARAR PARA COMPARLO CON EL NEABLE ALTO DEL DATO ORIGINAL
    CMP M                   ;COMPARAMOS SI EL NEABLE A COMPARAR ES MENOR AL NEABLE ALTO DEL DATO MULTIPLICADO (A<M -> C=1 Y SALTA)
  JZ Iguales
    JC NeablePequeÃ±o100       ;DEPENDIENDO DE LA ITERACION PODEMOS ACABAR O PROSEGUIR CONEL CICLO
        BajosIguales: ANA A

        VUELVE:MVI C, 54H             ;INICIAMOS &BC EN 1054, AQUI Y EN 1055 GUARDAREMOS EL NUMERO A SUMAR
        ;SUMAMOS EL NEABLE BAJO DEL NUMERO A SUMAR
        ANA A                 ;<---- POR SI ACASO
        LDAX B                ;TRAEMOS EL CONTENIDO DEL NEABLE BAJO DE B PARA SUMARLO CON
        MVI L, 61H
        ADD M                 ;SUMAMOS EL 9CH DEL NEABLE BAJO DE 156
        STAX B                ;ENVIAMOS EL NUMERO SUMADO AL NEABLE BAJO

        ;SUMAMOS EL ACARREO QUE DEJO EL NEABLE BAJO AL NEABLE ALTO
        INR C    ;!           ;INCREMENTAMOS &DE PARA USAR EL NEABLE ALTO
        LDAX B                ;TRAEMOS EL CONTENIDO DEL NEABLE ALTO PARA SUMARLO CON EL ACARREO
        ACI 00H               ;SUMAMOS 00H + EL CONTENIDO DEL ACUMULADOR + EL ACARREO QUE DEJO EL NEABLE BAJO (SI LO HAY)
        STAX B                ;ENVIAMOS EL NUMERO SUMADO + EL ACARREO DEL NEABLE BAJO (SI LO HUBO) AL NEABLE ALTO

        ;Guardamos el numero en la posicion que necesitamos
        MVI C, 58H
        MVI E, 52H
        LDAX B
        STAX D
        INR C
        INR E
        LDAX B
        STAX D

      JMP C100          ;Nos de volvemos a C1
    Iguales: ANA A

        ;COMPARAR LOS NEABLES BAJOS
        MVI E, 58H
        MVI L, 50H          ;TRAEMOS EL NEABLE ALTO DEL NUMERO MULTIPLICADO PARA COMPARARLOS PRIMERO
        ;XCHG                    ;CAMBIE  DE CON HL
        LDAX D                  ;TRAEMOS EL NEABLE ALTO DEL NUMERO A COMPARAR PARA COMPARLO CON EL NEABLE ALTO DEL DATO ORIGINAL
        CMP M                   ;COMPARAMOS SI EL NEABLE A COMPARAR ES MENOR AL NEABLE ALTO DEL DATO MULTIPLICADO (A<M -> C=1 Y SALTA)
      JZ BajosIguales
    JC VUELVE

NeablePequeÃ±o100:RET
;>>>>>FIN CICLO DE 100 EN 100<<<<<<<<<<




;>>>>>>>CICLO DE 10 EN 10 ESPECIAL<<<<<<<<<<
CICLO10SPECIAL: MVI L,50H
 MOV B,M			                   ;Guardamos el dato en el registro B
 MVI D,0AH                       ;Inicializamos el registro D con 0AH para los limites de 10

 C10SPECIAL:MOV A,B		           ;Recuperamos el dato de B en el acumulador (necesario para lo siguientes pasos)
  CMP D                          ;Comparamos con D, si es menor saltamos al final y sumamos C que
 JC C102SPECIAL				           ;idealmente C debe valer cero para este caso

 MOV A,C	                       ;Movemos el contenido de C al acumulador
	ADI 06H                        ;Sumamos 06H a este contenido
 MOV C,A                         ;Guardamos el resultado en C nuevamente
 MOV A,D                         ;recuperamos el valor de D
	ADI 0AH                        ;y le sumamos 0AH para la siguiente iteracion
	MOV D,A                        ;Nuevamente guardamos el valor
 JMP C10SPECIAL                  ;Nos de volvemos a C1

 C102SPECIAL: RET
;>>>>>>>FIN CICLO DE 10 EN 10 ESPECIAL<<<<<<<<<<


;>>>>>>>SUMAR RESULTADO<<<<<<<<<<
SumarResultado:MVI L,50H    ;APUNTAMOS A LA UBICACION DE NEABLE BAJO DEL NUMERO A CONVERTIR
  MVI C,54H                 ;APUNTAMOS AL NEABLE BAJO DEL NUMERO A SUMAR
  LDAX B                    ;TRAEMOS EL CONTENIDO DEL NEABLE BAJO DEL NUMERO A SUMAR
  ADD M                     ;SUMAMOS EL NEABLE BAJO DEL NUMERO A SUAMAR CON EL NEABLE BAJO DEL NUMERO A CONVERTIR
  MOV M, A                  ;MOVEMOS EL RESULTADO A 1050

  ;POR SI HAY ACARREO EN LA SUMA DE LOS NEABLES BAJOS
  INX H                     ;INCREMENTAMOS &HL PARA USAR EL NEABLE ALTO DEL NUMERO A CONVERTIR
  MOV A,M                   ;TRAEMOS EL CONTENIDO DEL NEABLE ALTO DEL NUMERO A CONVERTIR PARA SUMARLO CON EL ACARREO
  ACI 00H                   ;SUMAMOS 00H + EL CONTENIDO DEL ACUMULADOR + EL ACARREO QUE DEJO EL NEABLE BAJO (SI LO HAY)


  MVI L, 55H                ;APUNTAMOS CON HL AL NEABLE ALTO DEL NUMERO A SUMAR
  ADD M                     ;SUMAMOS EL CONTENIDO DEL NEABLE ALTO DEL NUMERO A SUMAR+EL ACARREO+EL NUMERO CONVERTIDO
  MVI L, 51H                ;APUNTAMOS A LA POSICION DEL NEABLE ALTO DEL NUMERO A CONVERTIR
  MOV M, A                  ;GUARDAMOS EL RESULTADO DE LAS SUMAS DE NEABLES ALTOS EN &HL 1051

ret
;>>>>>>>FIN SUMAR RESULTADO<<<<<<<<<<



;>>>>>>>>>>ROTACION DE NUMEROS<<<<<<<<<<<<<<<
VISUALIZACION: LXI H,D0               ;VOLVEMOS A LA POSICION DONDE GUADAMOS LOS DATOS DEL CERO MATRICIAL
 ADD H	                              ;SUMAMOS EL REGISTRO H AL A PARA UBICAR LA MATRIZ DE DATOS A COPIAR
 MOV H,A	                            ;Movemos el resultado de la suma a H, para tener la direccion con el numero indicado
	LXI D,8000H                         ;Direccionamos D para guardar una copia de lo direccionado por H
	MVI B,06H                           ;Ponemos un 6 para iterar todos los datos en &(HL)

COPIA: MOV A,M                        ;Ponemos en el acumulador el dato direccionado por &(HL)
 STAX D 															;Hacemos una copia de lo que hay en A en la memoria &(DE), que esta en 8000H
 INX H 																;AUMENTAMOS H
 INX D	                              ;AUMENTAMOS D
 DCR B                                ;DECREMENTAMOS B
JNZ COPIA															;Seguimos hasta que B sea cero (hasta que hayamos recorrido las filas)

;UNA VEZ GUARDADA LA LISTA, RECORREMOS FILAS Y COLUMNAS PARA ROTAR LOS DATOS

	MVI B,04H                           ;Movemos 04H a B para iterarlas columnas
COLUMNA: MVI C,06H                    ;MOVEMOS EL DATO 06 A C PARA UNA ITERACION DE COLUMNAS
  LXI D, 8000H	                      ;VOLVEMOS A LA POSICION DONDE HICIMOS LA COPIA
	LXI H,9000H                         ;Iniciamos en la posicion 9000H, aqui guardaremos los numeros rotados
C2: LDAX D	                          ;Cargamos a A con el contenido direccionado por &(DE)
 RAL                                  ;ROTAMOS UNA VEZ A A LA IZQUIERDA CON ACARREO
 STAX D                               ;PASAMOS EL RESULTADO DE A A LA MEMORIA DE
 MOV M,A                              ;Guardamos tambien el resultado en la memoria &(HL)
 INX D                                ;INCREMENTAMOS D
 INX H                                ;INCREMENTAMOS H
 DCR C                                ;DECREMENTAMOS LAS FILAS C
 JNZ C2                               ;Comprobamos si ya hemos recorrido las filas osea C=0
CALL IMP                              ;Una vez rotados los datos, los mandamos a imprimir

 DCR B                                ;Cuando ya hemos imprido los datos, decrementamos B, para seguir
																			;iterando las columnas
 JNZ COLUMNA                          ;Comprobamos si ya hemos recorrido las Columnas osea B = 0
RET
;>>>>>>>>>>TERMINA ROTACION DE NUMEROS<<<<<<<<<<<<<<<



;>>>>>>>>>>IMPRESION DE NUMERO<<<<<<<<<<<<<<<
IMP: LXI H,9000H     ;Iniciamos en la posicion 0900H, ya que aqui tenemos los numeros rotados

		MOV A,M           ;se mueve el contenido direccionado por &(HL) a A
 		OUT 01H           ;Se muestra en el primer puerto
  	INX H             ;Se incrementa H
  	MOV A,M           ;Se mueve el contenido direccionado a A
		OUT 02H           ;Se muestra por puerto
		INX H             ;y se repite el proceso...
		MOV A,M
		OUT 03H
		INX H
		MOV A,M
		OUT 04H
		INX H
		MOV A,M
		OUT 05H
		INX H
		MOV A,M
		OUT 06H    ;Hasta aqui

RET
;>>>>>>>>>>TERMINA IMPRESION DE NUMERO<<<<<<<<<<<<<<<


;>>>>>>>>>>CONVERSOR A 1000<<<<<<<<<<<<<<<
CONVERSOR1000: MVI L, 51H                 ;APUNTAMOS AL NEABLE ALTO PARA COMPARARLO MAS ADELANTE
  MVI D, 10H                              ;APUNTAMOS D A 10H PARA USARLO COMO APUNTADOR MAS ADELANTE
  MVI B, 10H                              ;APUNTAMOS B A 10H PARA USARLO COMO APUNTADOR MAS ADELANTE

  MVI A,03H                               ;PONEMOS UN CERO EN EL ACUMULADOR
  CMP M                                   ;COMPARAMOS A CON EL NEABLE ALTO PARA SABER SI ESTE VALE cero (A<M -> C=1 Y SALTA)
  JC NeableCero                           ;ENTRARA SI EL NEABLE ALTO ES IGUAL A CERO, DE LO CONTRAIO SALTARA
    JZ MenorA1000                         ;SALTA SI EL NEABLE ALTO ES IGUAL A 03H
      ;AQUI ADENTRO SABEMOS QUE EL NEABLE ALTO VALE CERO, POR LO TANTO EL MAXIMO NUMERO
      ;EN EL NEABLE BAJO ES 255, CON ESTO PODEMO EMPEZAR APREGUNTAR DESDE AQUI
      MVI A, 00H                          ;SI ES MENOR O IGUAL A CERO ENTRA
      CMP M
      JC ALTOCERO

      MVI L,50H                           ;DECREMENTAMOS L PARA OBTENER EL NEABLE BAJO
      MOV A, M                            ;TRAEMOS EL NEABLE BAJO AL ACUMULADOR
      CPI 64H                             ;COMPARAMOS SI ES MENOR A 100H  (A<v -> C=1 Y SALTA)
      JNC Menor100                        ;SI EL NUMERO ES MENOR A 100 ENTRA EN EL CICLO, SINO SALTA

        CALL MOSTRARMENORA100             ;LLAMAMOS EL ALGORITMO PARA VIZUALIZAR RESULTADOS MENORES A 100 O 10


     MenorA1000: ANA A                    ;ACA COMPROBAREMOS SI EL NEABLE BAJO ES MENOR E8 (OSEA MENOR A 1000)
        MVI L, 50H                        ;APUNTAMOS AL NEABLE BAJO
        MVI A, E8H                        ;GUARDAMOS EN EL ACUMULADOR E8 PARA COMPARAR CON EL NEABLE BAJO
        CMP M                             ;COMPARAMOS SI EL NUMERO ES MENOR A 100
        JC MAYORA999                      ;SI ES MAYOR A 1000 SALTA
      Menor100:   ANA A
      ALTOCERO:   MVI L, 60H
        MVI M, 64H
        INR L
        MVI M, 9CH
        CALL CICLO100Y10

        ;actualizamos el dato temporal
        MVI C, 52H
        MVI E, 58H
        LDAX B
        STAX D
        INX B
        INX D
        LDAX B
        STAX D

        MVI L, 60H
        MVI M, 0AH
        INR L
        MVI M, 06H
        CALL CICLO100Y10                      ;EL RESULTADO DE C PODEMOS SUMARLO AL NEABLE BAJO DEL RESULTADO

        CALL SumarResultado                   ;SUMAMOS EL NUMERO A SUMAR CON EL NUMERO A CONVERTIR PARA CONVERITRLO


        ;ROTAMOS Y VIZUALIZAMOS UN RESULTADO QUE SIEMPRE SERA DE 3 DIGITOS
          MVI L, 51H                          ;PONEMOS LA POSICION DEL NEABLE ALTO PARA MOSTRARLO PRIMERO

         ;ROTAMOS EL NEABLE ALTO
          MOV A,M                             ;Lo pasamos al acumulador
        	ANI 0FH                             ;le Hacemos una mascara
         CALL VISUALIZACION                   ;Llamamos a la funcion para rotar

         ;ROTAMOS EL NEABLE BAJO
         MVI H, 10H
         MVI L, 50H
         MOV A,M			 										    ;TRAEMOS EL DATO DEL NEABLE ALTO AL ACUMULADOR(YA QUE PRIMERO DEBEMOS ROTAR EL NEABLE ALTO)
        	ANI F0H													    ;hacemos una mascara para los bits de menor peso
         RRC 							                    ;ROTAMOS 4 VECES A LA DERECHA SIN ACARREO
         RRC                                  ;ROTAMOS HACIA LA DERECHA PARA PONER EL DATO EN
         RRC 																  ;ENABLE BAJO
        	RRC
         CALL VISUALIZACION                   ;Llamamos a la funcion para rotar
          MVI H, 10H
        	MVI L, 50H                          ;RECUPERAMOS EL DATO PARA ROTAR Y MOSTRAR SUS PRIMEROS 4 BITS
          MOV A,M                             ;Lo pasamos al acumulador
        	ANI 0FH                             ;le Hacemos una mascara
         CALL VISUALIZACION                   ;Llamamos a la funcion para rotar

        HLT

      MAYORA999: ANA A                        ;SI EL NUMERO ES MAYOR A 1000 LO DIRECCIONAMOS ACA PARA HACER EL ALGORITMO DE 1000
  NeableCero: CALL CICLO1000                  ;PONER AQUI COMPROBACION PARA VER SI ES MENOR A MIL, SINO HACER CICLO1000

  ;Guardamos el numero en la posicion que necesitamos
  MVI C, 52H
  MVI E, 58H
  LDAX B
  STAX D
  INX B
  INX D
  LDAX B
  STAX D

  MVI L, 60H
  MVI M, 64H
  INR L
  MVI M, 9CH
  CALL CICLO100Y10                              ;LUEGO DE LLAMAR AL ALGORITMO DE 1000 EN 1000 LLAMAMOS AL DE 100 EN 100

  ;actualizamos el dato temporal
  MVI C, 52H
  MVI E, 58H
  LDAX B
  STAX D
  INX B
  INX D
  LDAX B
  STAX D

  MVI L, 60H
  MVI M, 0AH
  INR L
  MVI M, 06H
  CALL CICLO100Y10                               ;POR ULTIMO EJECUTAMOS EL CICLO DE 10 EN 10


  CALL SumarResultado                            ;Y SUMAMOS EL NUMERO A SUMAR CON EL NUMERO A CONVERTIR

;LLAMAMOS AL ALGORITMOS PARA VIZUALIZAR 4 DIGITOS YA QUE SIEMPRE HABRAN 4 DIGITOS
    MVI L, 51H                                   ;APUNTAMOS AL NEABLE ALTO PARA MOSTRARLO PRIMERO
    ;ROTAMOS EL NEABLE ALTO
    MOV A,M			 										             ;TRAEMOS EL DATO DEL NEABLE ALTO AL ACUMULADOR(YA QUE PRIMERO DEBEMOS ROTAR EL NEABLE ALTO)
     ANI F0H													           ;hacemos una mascara para los bits de menor peso
    RRC 							                           ;ROTAMOS 4 VECES A LA DERECHA SIN ACARREO
    RRC                                          ;ROTAMOS HACIA LA DERECHA PARA PONER EL DATO EN
    RRC 											        					 ;ENABLE BAJO
     RRC
    CALL VISUALIZACION                           ;Llamamos a la funcion para rotar
     MVI H, 10H
     MVI L, 51H                                  ;RECUPERAMOS EL DATO PARA ROTAR Y MOSTRAR SUS PRIMEROS 4 BITS
     MOV A,M                                     ;Lo pasamos al acumulador
     ANI 0FH                                     ;le Hacemos una mascara
    CALL VISUALIZACION                           ;Llamamos a la funcion para rotar

    ;ROTAMOS EL NEABLE BAJO
    MVI H, 10H
    MVI L, 50H
    MOV A,M			 										             ;TRAEMOS EL DATO DEL NEABLE ALTO AL ACUMULADOR(YA QUE PRIMERO DEBEMOS ROTAR EL NEABLE ALTO)
     ANI F0H											               ;hacemos una mascara para los bits de menor peso
    RRC 							                           ;ROTAMOS 4 VECES A LA DERECHA SIN ACARREO
    RRC                                          ;ROTAMOS HACIA LA DERECHA PARA PONER EL DATO EN
    RRC 																         ;ENABLE BAJO
     RRC
    CALL VISUALIZACION                           ;Llamamos a la funcion para rotar
     MVI H, 10H
     MVI L, 50H                                  ;RECUPERAMOS EL DATO PARA ROTAR Y MOSTRAR SUS PRIMEROS 4 BITS
     MOV A,M                                     ;Lo pasamos al acumulador
     ANI 0FH                                     ;le Hacemos una mascara
    CALL VISUALIZACION                           ;Llamamos a la funcion para rotar

    HLT                              ;forzamos el cierre del programa

RET
;>>>>>>>>>>TERMINA CONVERSOR A 1000<<<<<<<<<<<<<<<


;>>>>>>>>>>TERMINA MOSTRAR RESTA<<<<<<<<<<<<<<<
MOSTRARMENORA100:CPI 0AH                                   ;COMPARAMOS SI EL RESULTADO ES MENOR A 10
    JC RESTAMENOR10                           ;SALTAMOS SI EL RESULTADO ES MENOR A 10
      ;RESULTADO MAYOR A 10 (2 DIGITOS)
      MVI C, 00H                                ;INICIALIZAMOS C EN 00H
      CALL CICLO10SPECIAL
      MOV A, C                                  ;OBTENEMOS EL DATO A Sumar
      ADD M                                     ;SE LO SUMAMOS AL NEABLE BAJO 1050H
      MOV M, A                                  ;GUARDAMOS EL DATO EN 1050H

      ;VIZUALIZAMOS EL RESULTADO DE DOS DIGITOS
     	ANI F0H													          ;hacemos una mascara para los bits de menor peso
      RRC 							                        ;ROTAMOS 4 VECES A LA DERECHA SIN ACARREO
      RRC                                       ;ROTAMOS HACIA LA DERECHA PARA PONER EL DATO EN
      RRC 																      ;ENABLE BAJO
     	RRC
      CALL VISUALIZACION                        ;Llamamos a la funcion para rotar
       MVI H, 10H
     	MVI L, 50H                                ;RECUPERAMOS EL DATO PARA ROTAR Y MOSTRAR SUS PRIMEROS 4 BITS
       MOV A,M                                  ;Lo pasamos al acumulador
     	ANI 0FH                                   ;le Hacemos una mascara
      CALL VISUALIZACION                        ;Llamamos a la funcion para rotar

      HLT                                       ;TERMINAMOS LA EJECUCION

    RESTAMENOR10:MOV A,M                       ;Lo pasamos al acumulador
      ;AQUI MOSTRAREMOS EL EESULTADO DE LA RESTA QUE DE 1 DIGITO
      ANI 0FH                                   ;le Hacemos una mascara
      CALL VISUALIZACION                        ;Llamamos a la funcion para rotar

      HLT                                       ;TERMINAMOS EJECUCIÓN
RET
;>>>>>>>>>>TERMINA MOSTRAR RESTA<<<<<<<<<<<<<<<



.END
