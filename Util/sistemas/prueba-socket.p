    &SCOPED-DEFINE HTTP-NEWLINE CHR(13) + CHR(10)
    &SCOPED-DEFINE RESPONSE-TIMEOUT 45 
    
    DEFINE VARIABLE vSocket             AS HANDLE NO-UNDO.   
    DEFINE VARIABLE vloop               AS LOGICAL NO-UNDO.
    DEFINE VARIABLE pRESPONSE   AS LONGCHAR NO-UNDO.

    DEFINE VAR s-codcia AS INTE INIT 001.
    DEFINE VAR pMessage AS CHAR NO-UNDO.
    DEFINE VAR pArtCode AS CHAR NO-UNDO.
    DEFINE VAR pSalesChannel AS CHAR INIT '6'.
    DEFINE VAR pCategoryCustomer AS CHAR INIT 'C'.
    DEFINE VAR pSalesCondition AS CHAR INIT '000'.
    DEFINE VAR x-Url AS CHAR NO-UNDO.
    DEFINE VAR x-Texto AS CHAR.
    DEF VAR x-Cadena1 AS CHAR NO-UNDO.
    DEF VAR x-Cadena2 AS CHAR NO-UNDO.
    DEF VAR x-Inicio AS INTE NO-UNDO.
    DEF VAR x-Fin AS INTE NO-UNDO.

    FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia
        AND VtaTabla.Tabla = 'CONFIG-WEB-PRICING' NO-LOCK NO-ERROR.
    IF NOT AVAILABLE VtaTabla THEN DO:
        MESSAGE "NO se encontró la configuración CONFIG-WEB-PRICING".
        RETURN "ADM-ERROR".
    END.

    /*OPEN THE SOCKET*/
    CREATE SOCKET vSocket.
    vSocket:SET-READ-RESPONSE-PROCEDURE ("readHandler",THIS-PROCEDURE).



    DEF VAR x-Resultado AS CHAR NO-UNDO.
    DEF VAR x-Respuesta AS LONGCHAR NO-UNDO.
    DEF VAR x-Contenido AS LONGCHAR NO-UNDO.
    DEFINE VAR cHOST        AS CHAR NO-UNDO.
    DEFINE VAR cPORT        AS CHAR NO-UNDO.
    DEFINE VAR cURL         AS CHAR NO-UNDO.
    DEFINE VARIABLE requestString       AS CHAR   NO-UNDO.
    DEFINE VARIABLE wStatus             AS LOGICAL NO-UNDO.

    INPUT FROM d:\newtest.txt.
    REPEAT:
        IMPORT UNFORMATTED pArtCode.
        IF TRUE <> (pArtCode > '') THEN LEAVE.
        /* Capturamos URL y TOKEN */
        x-Url = TRIM(VtaTabla.Llave_c1) +       /* URL */
                TRIM(VtaTabla.Llave_c2) .       /* Token */
        /* Llave de búsqueda */
        x-Url = x-Url + ~
        '/' + pArtCode + ~
        '/' + pSalesChannel + ~
        '/' + pCategoryCustomer + ~
        '/' + pSalesCondition.

        RUN UrlParser(INPUT x-URL, OUTPUT cHOST, OUTPUT cPORT, OUTPUT cURL).
        
        ASSIGN requestString = "GET " + cURL + " HTTP/1.0" + {&HTTP-NEWLINE} +
                  "Accept: */*" + {&HTTP-NEWLINE} + 
                  "Host: " + chost + {&HTTP-NEWLINE} + 
                  /*"Connection: Keep-Alive" + {&HTTP-NEWLINE} + */
                  {&HTTP-NEWLINE}.
        ASSIGN wstatus = vSocket:CONNECT("-H " + chost + " -S " + cport) NO-ERROR.
        /*Now make sure the socket is open*/
        IF wstatus = NO THEN DO:
            MESSAGE  "0:No Socket".
            RETURN 'ADM-ERROR'.
        END.

        RUN http-get-contenido (INPUT x-Url,
                                OUTPUT x-Resultado,
                                /*OUTPUT x-Respuesta,*/
                                OUTPUT x-Contenido).      /* Respuesta del API */
    END.
    INPUT CLOSE.
    vSocket:DISCONNECT().
    DELETE OBJECT vSocket.

RETURN.

/* ************************** */
PROCEDURE http-get-contenido:
/* ************************** */

    DEFINE INPUT PARAMETER pURL AS CHAR.
    DEFINE OUTPUT PARAMETER pRESULT     AS CHAR NO-UNDO.
    /*DEFINE OUTPUT PARAMETER pRESPONSE   AS LONGCHAR NO-UNDO.*/
    DEFINE OUTPUT PARAMETER pContent    AS LONGCHAR NO-UNDO.

    
    DEFINE VARIABLE vBuffer             AS MEMPTR NO-UNDO.
    DEFINE VARIABLE vPackets            AS INTEGER NO-UNDO.
    
    DEFINE VAR path AS CHAR.
    DEFINE VAR host AS CHAR.
    
    
    /* Got socket - Now make HTTP request */
    SET-SIZE(vBuffer) = LENGTH(requestString) + 1.
    PUT-STRING(vBuffer,1) = requestString.
    vSocket:WRITE(vBuffer, 1, LENGTH(requestString)).
    SET-SIZE(vBuffer) = 0.
    
    /*Wait for a response*/
    ASSIGN vloop = TRUE.  /*Turns off automatically when request is done*/
    DEFINE VAR vstarttime AS INTEGER.
    ASSIGN vstarttime = etime.
    
    MESSAGE partcode 'inicio'.
    WAITLOOP:
    DO WHILE vloop:
        PROCESS EVENTS.
        PAUSE 0.    /* 1 */
        /* Build in timer in case sending is never set to NO
           this will terminate the program after 60 seconds
           start-Etime will be reset by WriteData each time there
           is activity on the socket to allow for long transmissions */
        IF vstarttime + ({&RESPONSE-TIMEOUT} * 1000) < ETIME
         THEN DO:
            MESSAGE "timed out at " + string(etime - vstarttime) + " msec".
            ASSIGN pResult = "0:Failure".
            RETURN 'ADM-ERROR'.
        END. /*No Response, or timed out*/
    END.
    MESSAGE partcode 'fin'.
    
    
    /*At this point, pResponse should be populated with the result (up to 32K)*/
    
    /*All Done!*/
    ASSIGN pResult = "1:Success".
    ASSIGN 
     pContent = SUBSTRING(pResponse,INDEX(pResponse,{&HTTP-NEWLINE} + {&HTTP-NEWLINE}),-1).
     
    ASSIGN
     pResponse = SUBSTRING(pResponse,1,INDEX(pResponse,{&HTTP-NEWLINE} + {&HTTP-NEWLINE})).


/* ************************** */
END PROCEDURE.
/* ************************** */


/* ************************** */
PROCEDURE readHandler:
/* ************************** */
    DEFINE VARIABLE bytesAvail  AS INTEGER  NO-UNDO.
    DEFINE VARIABLE b           AS MEMPTR   NO-UNDO.
    DEFINE VARIABLE lastBytes   AS INTEGER  NO-UNDO.

    IF vSocket:connected() THEN ASSIGN bytesAvail = vSocket:GET-BYTES-AVAILABLE().

    IF bytesAvail = 0 THEN DO: /*Todo OK / All Done*/
      ASSIGN vloop = FALSE.
      RETURN.
    END.

    /*
        OK, there's something on the wire... Read it in
        OK, hay algo en el trama... leerlo
    */    
    SET-SIZE(b) = bytesAvail + 1.
    vSocket:READ(b, 1, bytesAvail, 1).
    ASSIGN pResponse = GET-STRING(b,1).
    SET-SIZE(b) = 0.

/* ************************** */
END PROCEDURE. /*readHandler*/
/* ************************** */

/* ************************** */
PROCEDURE UrlParser:
/* ************************** */

    DEFINE INPUT PARAMETER purl AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER phost AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pport AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ppath AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE vStr AS CHARACTER NO-UNDO.    

    IF purl BEGINS "http://" THEN DO:
        vStr = SUBSTRING(purl, 8).
        phost = ENTRY(1, vStr, "/").

        IF NUM-ENTRIES(vStr, "/") = 1 THEN vStr = vStr + "/".

        ppath = SUBSTRING(vStr, INDEX(vStr,"/")).
        IF NUM-ENTRIES(phost, ":") > 1 THEN DO:
            pport = ENTRY(2, phost, ":").
            phost = ENTRY(1, phost, ":").
        END.
        ELSE DO:
            pport = "80".
        END.
    END.
    ELSE DO:
        phost = "".
        pport = "".
        ppath = purl.
    END.

/* ************************** */
END PROCEDURE.
/* ************************** */
