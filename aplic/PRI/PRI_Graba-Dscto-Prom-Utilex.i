DEFINE INPUT PARAMETER pReemplazar AS LOG.
DEFINE INPUT PARAMETER pCodMat AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR {&t-Tabla}.
DEFINE INPUT PARAMETER x-MetodoActualizacion AS INTE.
DEFINE INPUT PARAMETER f-Division AS CHAR.
DEFINE OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

DEFINE VAR pCuenta AS INTE NO-UNDO.
    
/* 14/11/2022 Se crea el registro de control */
FIND Almmmatg WHERE Almmmatg.codcia = s-codcia AND
    Almmmatg.codmat = pCodMat NO-LOCK NO-ERROR.
FIND FIRST VtaListaMinGn WHERE VtaListaMinGn.codcia = s-codcia AND
    VtaListaMinGn.codmat = pCodMat NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaListaMinGn THEN DO:
    CREATE VtaListaMinGn.
    ASSIGN
        VtaListaMinGn.CodCia = Almmmatg.codcia
        VtaListaMinGn.codmat = Almmmatg.codmat
        VtaListaMinGn.Chr__01 = Almmmatg.CHR__01
        VtaListaMinGn.UserUpdate = s-user-id 
        VtaListaMinGn.DateUpdate = TODAY.
END.
CASE pReemplazar:
    WHEN NO THEN DO:
        FOR EACH {&t-Tabla} EXCLUSIVE-LOCK WHERE {&t-Tabla}.CodCia = s-CodCia
            AND {&t-Tabla}.CodMat = pCodMat:
            IF x-MetodoActualizacion = 2 AND {&t-Tabla}.CodDiv <> f-Division THEN NEXT.
            FIND {&Tabla} WHERE {&Tabla}.CodCia = {&t-Tabla}.CodCia AND
                {&Tabla}.CodDiv = {&t-Tabla}.CodDiv AND
                {&Tabla}.CodMat = {&t-Tabla}.CodMat AND
                {&Tabla}.FchIni = {&t-Tabla}.FchIni AND
                {&Tabla}.FchFin = {&t-Tabla}.FchFin AND
                {&Tabla}.FlgEst = "A"
                NO-LOCK NO-ERROR.
            IF AVAILABLE {&Tabla} THEN DO:
                FIND CURRENT {&Tabla} EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                IF ERROR-STATUS:ERROR = YES THEN DO:
                    {lib/mensaje-de-error.i &CuentaError="pCuenta" &MensajeError="pMensaje"}
                    UNDO, NEXT.
                END.
                /* Siempre se va a INACTIVAR */
                ASSIGN
                    {&Tabla}.FlgEst = "I"
                    {&Tabla}.FchAnulacion = TODAY
                    {&Tabla}.HoraAnulacion = STRING(TIME, 'HH:MM:SS')
                    {&Tabla}.UsrAnulacion = s-user-id.
            END.
            /* Siempre se crea un registro */
            CREATE {&Tabla}.
            ASSIGN
                {&Tabla}.codcia = {&t-Tabla}.CodCia 
                {&Tabla}.FlgEst = "A"
                {&Tabla}.CodDiv = {&t-Tabla}.CodDiv
                {&Tabla}.CodMat = {&t-Tabla}.CodMat 
                {&Tabla}.FchIni = {&t-Tabla}.FchIni 
                {&Tabla}.FchFin = {&t-Tabla}.FchFin 
                {&Tabla}.FchCreacion = TODAY
                {&Tabla}.HoraCreacion = STRING(TIME, 'HH:MM:SS')
                {&Tabla}.UsrCreacion = s-user-id
                {&Tabla}.Tipo = "CONVIVIR".
            ASSIGN
                {&Tabla}.Descuento    = {&t-Tabla}.Descuento 
                {&Tabla}.DescuentoMR  = {&t-Tabla}.DescuentoMR 
                {&Tabla}.DescuentoVIP = {&t-Tabla}.DescuentoVIP 
                .
            DELETE {&t-Tabla}.
        END.
    END.
    WHEN YES THEN DO:
        FOR EACH {&t-Tabla} EXCLUSIVE-LOCK WHERE {&t-Tabla}.CodCia = s-CodCia
            AND {&t-Tabla}.CodMat = pCodMat:
            IF x-MetodoActualizacion = 2 AND {&t-Tabla}.CodDiv <> f-Division THEN NEXT.
            /* ****************************************************************************************** */
            /* 1ro. Buscamos promoci�n que cuya fecha de inicio se encuentre dentro de la nueva promoci�n */
            /* ****************************************************************************************** */
            FOR EACH {&b-Tabla} EXCLUSIVE-LOCK WHERE {&b-Tabla}.CodCia = {&t-Tabla}.CodCia
                AND {&b-Tabla}.CodDiv = {&t-Tabla}.CodDiv
                AND {&b-Tabla}.CodMat = {&t-Tabla}.CodMat
                AND {&b-Tabla}.FlgEst = "A"
                AND {&b-Tabla}.FchIni >= {&t-Tabla}.FchIni 
                AND {&b-Tabla}.FchIni <= {&t-Tabla}.FchFin:
                IF {&b-Tabla}.FchFin <= {&t-Tabla}.FchFin THEN DO:
                    /* La promoci�n se encuentra dentro de la nueva promoci�n => es absorvida por la nueva promoci�n */
                    ASSIGN
                        {&b-Tabla}.FlgEst = "I"
                        {&b-Tabla}.FchAnulacion = TODAY 
                        {&b-Tabla}.UsrAnulacion = s-user-id
                        {&b-Tabla}.HoraAnulacion = STRING(TIME, 'HH:MM:SS').
                END.
                ELSE DO:
                    /* Se inactiva y se genera una nueva con nuevos vencimientos */
                    CREATE {&Tabla}.
                    BUFFER-COPY {&b-Tabla} TO {&Tabla}
                        ASSIGN
                        {&Tabla}.FlgEst = "A"
                        {&Tabla}.FchIni = {&t-Tabla}.FchFin + 1       /* OJO */
                        {&Tabla}.HoraCreacion = STRING(TIME, 'HH:MM:SS')
                        {&Tabla}.FchCreacion = TODAY
                        {&Tabla}.UsrCreacion = s-user-id.
                    ASSIGN
                        {&b-Tabla}.FlgEst = "I"
                        {&b-Tabla}.FchAnulacion = TODAY 
                        {&b-Tabla}.UsrAnulacion = s-user-id
                        {&b-Tabla}.HoraAnulacion = STRING(TIME, 'HH:MM:SS').
                END.
            END.
            /* ****************************************************************************************** */
            /* 2do. Buscamos promoci�n que cuya fecha de fin sea >= de la de fin de la nueva promoci�n    */
            /* ****************************************************************************************** */
            FOR EACH {&b-Tabla} EXCLUSIVE-LOCK WHERE {&b-Tabla}.CodCia = {&t-Tabla}.CodCia
                AND {&b-Tabla}.CodDiv = {&t-Tabla}.CodDiv
                AND {&b-Tabla}.CodMat = {&t-Tabla}.CodMat
                AND {&b-Tabla}.FlgEst = "A"
                AND {&b-Tabla}.FchIni < {&t-Tabla}.FchIni 
                AND {&b-Tabla}.FchFin >= {&t-Tabla}.FchIni:
                /* Se inactiva y se generan hasta 2 una con nuevos vencimientos */
                CREATE {&Tabla}.
                BUFFER-COPY {&b-Tabla} TO {&Tabla}
                    ASSIGN
                    {&Tabla}.FlgEst = "A"
                    {&Tabla}.FchFin = {&t-Tabla}.FchIni - 1       /* OJO */
                    {&Tabla}.HoraCreacion = STRING(TIME, 'HH:MM:SS')
                    {&Tabla}.FchCreacion = TODAY
                    {&Tabla}.UsrCreacion = s-user-id.
                IF {&b-Tabla}.FchFin > {&t-Tabla}.FchFin THEN DO:
                    CREATE {&Tabla}.
                    BUFFER-COPY {&b-Tabla} TO {&Tabla}
                        ASSIGN
                        {&Tabla}.FlgEst = "A"
                        {&Tabla}.FchIni = {&t-Tabla}.FchFin + 1       /* OJO */
                        {&Tabla}.HoraCreacion = STRING(TIME, 'HH:MM:SS')
                        {&Tabla}.FchCreacion = TODAY
                        {&Tabla}.UsrCreacion = s-user-id.
                END.
                ASSIGN
                    {&b-Tabla}.FlgEst = "I"
                    {&b-Tabla}.FchAnulacion = TODAY 
                    {&b-Tabla}.UsrAnulacion = s-user-id
                    {&b-Tabla}.HoraAnulacion = STRING(TIME, 'HH:MM:SS').
            END.
            /* ****************************************************************************************** */
            /* 3ro. Grabamos la nueva promoci�n */
            /* ****************************************************************************************** */
            CREATE {&Tabla}.
            ASSIGN
                {&Tabla}.codcia = {&t-Tabla}.CodCia 
                {&Tabla}.FlgEst = "A"
                {&Tabla}.CodDiv = {&t-Tabla}.CodDiv
                {&Tabla}.CodMat = {&t-Tabla}.CodMat 
                {&Tabla}.FchIni = {&t-Tabla}.FchIni 
                {&Tabla}.FchFin = {&t-Tabla}.FchFin 
                {&Tabla}.FchCreacion = TODAY
                {&Tabla}.HoraCreacion = STRING(TIME, 'HH:MM:SS')
                {&Tabla}.UsrCreacion = s-user-id
                {&Tabla}.Tipo = "REEMPLAZAR"
                {&Tabla}.Descuento    = {&t-Tabla}.Descuento 
                {&Tabla}.DescuentoMR  = {&t-Tabla}.DescuentoMR 
                {&Tabla}.DescuentoVIP = {&t-Tabla}.DescuentoVIP 
                .
            DELETE {&t-Tabla}.
        END.
    END.
END CASE.

/*
CASE pReemplazar:
    WHEN NO THEN DO:
        FOR EACH {&t-Tabla} EXCLUSIVE-LOCK WHERE {&t-Tabla}.CodCia = s-CodCia
            AND {&t-Tabla}.CodMat = pCodMat, 
            FIRST VtaListaMinGn OF {&t-Tabla} NO-LOCK:
            IF x-MetodoActualizacion = 2 AND {&t-Tabla}.CodDiv <> f-Division THEN NEXT.
            FIND {&Tabla} WHERE {&Tabla}.CodCia = {&t-Tabla}.CodCia AND
                {&Tabla}.CodDiv = {&t-Tabla}.CodDiv AND
                {&Tabla}.CodMat = {&t-Tabla}.CodMat AND
                {&Tabla}.FchIni = {&t-Tabla}.FchIni AND
                {&Tabla}.FchFin = {&t-Tabla}.FchFin AND
                {&Tabla}.FlgEst = "A"
                NO-LOCK NO-ERROR.
            IF AVAILABLE {&Tabla} THEN DO:
                FIND CURRENT {&Tabla} EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                IF ERROR-STATUS:ERROR = YES THEN DO:
                    {lib/mensaje-de-error.i &CuentaError="pCuenta" &MensajeError="pMensaje"}
                    UNDO, NEXT.
                END.
                /* Siempre se va a INACTIVAR */
                ASSIGN
                    {&Tabla}.FlgEst = "I"
                    {&Tabla}.FchAnulacion = TODAY
                    {&Tabla}.HoraAnulacion = STRING(TIME, 'HH:MM:SS')
                    {&Tabla}.UsrAnulacion = s-user-id.
            END.
            /* Siempre se crea un registro */
            CREATE {&Tabla}.
            ASSIGN
                {&Tabla}.codcia = {&t-Tabla}.CodCia 
                {&Tabla}.FlgEst = "A"
                {&Tabla}.CodDiv = {&t-Tabla}.CodDiv
                {&Tabla}.CodMat = {&t-Tabla}.CodMat 
                {&Tabla}.FchIni = {&t-Tabla}.FchIni 
                {&Tabla}.FchFin = {&t-Tabla}.FchFin 
                {&Tabla}.FchCreacion = TODAY
                {&Tabla}.HoraCreacion = STRING(TIME, 'HH:MM:SS')
                {&Tabla}.UsrCreacion = s-user-id
                {&Tabla}.Tipo = "CONVIVIR".
            ASSIGN
                {&Tabla}.Descuento    = {&t-Tabla}.Descuento 
                {&Tabla}.DescuentoMR  = {&t-Tabla}.DescuentoMR 
                {&Tabla}.DescuentoVIP = {&t-Tabla}.DescuentoVIP 
                {&Tabla}.Precio    = ROUND(VtaListaMinGn.PreOfi * (1 - ({&t-Tabla}.Descuento / 100)), 4)
                {&Tabla}.PrecioMR  = ROUND(VtaListaMinGn.PreOfi * (1 - ({&t-Tabla}.DescuentoMR / 100)), 4)
                {&Tabla}.PrecioVIP = ROUND(VtaListaMinGn.PreOfi * (1 - ({&t-Tabla}.DescuentoVIP / 100)), 4).
            DELETE {&t-Tabla}.
        END.
    END.
    WHEN YES THEN DO:
        FOR EACH {&t-Tabla} EXCLUSIVE-LOCK WHERE {&t-Tabla}.CodCia = s-CodCia
            AND {&t-Tabla}.CodMat = pCodMat, 
            FIRST VtaListaMinGn OF {&t-Tabla} NO-LOCK:
            IF x-MetodoActualizacion = 2 AND {&t-Tabla}.CodDiv <> f-Division THEN NEXT.
            /* ****************************************************************************************** */
            /* 1ro. Buscamos promoci�n que cuya fecha de inicio se encuentre dentro de la nueva promoci�n */
            /* ****************************************************************************************** */
            FOR EACH {&b-Tabla} EXCLUSIVE-LOCK WHERE {&b-Tabla}.CodCia = {&t-Tabla}.CodCia
                AND {&b-Tabla}.CodDiv = {&t-Tabla}.CodDiv
                AND {&b-Tabla}.CodMat = {&t-Tabla}.CodMat
                AND {&b-Tabla}.FlgEst = "A"
                AND {&b-Tabla}.FchIni >= {&t-Tabla}.FchIni 
                AND {&b-Tabla}.FchIni <= {&t-Tabla}.FchFin:
                IF {&b-Tabla}.FchFin <= {&t-Tabla}.FchFin THEN DO:
                    /* La promoci�n se encuentra dentro de la nueva promoci�n => es absorvida por la nueva promoci�n */
                    ASSIGN
                        {&b-Tabla}.FlgEst = "I"
                        {&b-Tabla}.FchAnulacion = TODAY 
                        {&b-Tabla}.UsrAnulacion = s-user-id
                        {&b-Tabla}.HoraAnulacion = STRING(TIME, 'HH:MM:SS').
                END.
                ELSE DO:
                    /* Se inactiva y se genera una nueva con nuevos vencimientos */
                    CREATE {&Tabla}.
                    BUFFER-COPY {&b-Tabla} TO {&Tabla}
                        ASSIGN
                        {&Tabla}.FlgEst = "A"
                        {&Tabla}.FchIni = {&t-Tabla}.FchFin + 1       /* OJO */
                        {&Tabla}.HoraCreacion = STRING(TIME, 'HH:MM:SS')
                        {&Tabla}.FchCreacion = TODAY
                        {&Tabla}.UsrCreacion = s-user-id.
                    ASSIGN
                        {&b-Tabla}.FlgEst = "I"
                        {&b-Tabla}.FchAnulacion = TODAY 
                        {&b-Tabla}.UsrAnulacion = s-user-id
                        {&b-Tabla}.HoraAnulacion = STRING(TIME, 'HH:MM:SS').
                END.
            END.
            /* ****************************************************************************************** */
            /* 2do. Buscamos promoci�n que cuya fecha de fin sea >= de la de fin de la nueva promoci�n    */
            /* ****************************************************************************************** */
            FOR EACH {&b-Tabla} EXCLUSIVE-LOCK WHERE {&b-Tabla}.CodCia = {&t-Tabla}.CodCia
                AND {&b-Tabla}.CodDiv = {&t-Tabla}.CodDiv
                AND {&b-Tabla}.CodMat = {&t-Tabla}.CodMat
                AND {&b-Tabla}.FlgEst = "A"
                AND {&b-Tabla}.FchIni < {&t-Tabla}.FchIni 
                AND {&b-Tabla}.FchFin >= {&t-Tabla}.FchIni:
                /* Se inactiva y se generan hasta 2 una con nuevos vencimientos */
                CREATE {&Tabla}.
                BUFFER-COPY {&b-Tabla} TO {&Tabla}
                    ASSIGN
                    {&Tabla}.FlgEst = "A"
                    {&Tabla}.FchFin = {&t-Tabla}.FchIni - 1       /* OJO */
                    {&Tabla}.HoraCreacion = STRING(TIME, 'HH:MM:SS')
                    {&Tabla}.FchCreacion = TODAY
                    {&Tabla}.UsrCreacion = s-user-id.
                IF {&b-Tabla}.FchFin > {&t-Tabla}.FchFin THEN DO:
                    CREATE {&Tabla}.
                    BUFFER-COPY {&b-Tabla} TO {&Tabla}
                        ASSIGN
                        {&Tabla}.FlgEst = "A"
                        {&Tabla}.FchIni = {&t-Tabla}.FchFin + 1       /* OJO */
                        {&Tabla}.HoraCreacion = STRING(TIME, 'HH:MM:SS')
                        {&Tabla}.FchCreacion = TODAY
                        {&Tabla}.UsrCreacion = s-user-id.
                END.
                ASSIGN
                    {&b-Tabla}.FlgEst = "I"
                    {&b-Tabla}.FchAnulacion = TODAY 
                    {&b-Tabla}.UsrAnulacion = s-user-id
                    {&b-Tabla}.HoraAnulacion = STRING(TIME, 'HH:MM:SS').
            END.
            /* ****************************************************************************************** */
            /* 3ro. Grabamos la nueva promoci�n */
            /* ****************************************************************************************** */
            CREATE {&Tabla}.
            ASSIGN
                {&Tabla}.codcia = {&t-Tabla}.CodCia 
                {&Tabla}.FlgEst = "A"
                {&Tabla}.CodDiv = {&t-Tabla}.CodDiv
                {&Tabla}.CodMat = {&t-Tabla}.CodMat 
                {&Tabla}.FchIni = {&t-Tabla}.FchIni 
                {&Tabla}.FchFin = {&t-Tabla}.FchFin 
                {&Tabla}.FchCreacion = TODAY
                {&Tabla}.HoraCreacion = STRING(TIME, 'HH:MM:SS')
                {&Tabla}.UsrCreacion = s-user-id
                {&Tabla}.Tipo = "REEMPLAZAR"
                {&Tabla}.Descuento    = {&t-Tabla}.Descuento 
                {&Tabla}.DescuentoMR  = {&t-Tabla}.DescuentoMR 
                {&Tabla}.DescuentoVIP = {&t-Tabla}.DescuentoVIP 
                {&Tabla}.Precio    = ROUND(VtaListaMinGn.PreOfi * (1 - ({&t-Tabla}.Descuento / 100)), 4)
                {&Tabla}.PrecioMR  = ROUND(VtaListaMinGn.PreOfi * (1 - ({&t-Tabla}.DescuentoMR / 100)), 4)
                {&Tabla}.PrecioVIP = ROUND(VtaListaMinGn.PreOfi * (1 - ({&t-Tabla}.DescuentoVIP / 100)), 4).
            DELETE {&t-Tabla}.
        END.
    END.
END CASE.
*/
