DEF VAR x-cargos AS CHAR.

x-cargos = 'Gerente de Finanzas,Gerente General,Jefe de Creditos,Jefe de Ventas,Atencion al Cliente,Asistente de Contabilidad,Gerente Comercial,Supermercados,Jefe de Supermercados,Ventas Provincia,Supervisor UTILEX,Administrador Tienda,Provincias,Supervisor de Ventas,Asistente de Gerencia 2,Jefe de Compras Corporativas'.

DEF VAR k AS INT.

OUTPUT TO d:\tmp\usuarios.txt.
PUT UNFORMATTED 'Usuario|Nombre|Seguridad' SKIP.
DO k = 1 TO NUM-ENTRIES(x-cargos):
    FOR EACH pf-g004 NO-LOCK WHERE aplic-id = 'vmy'
        AND INDEX(seguridad, ENTRY(k,x-cargos)) > 0,
        FIRST _user NO-LOCK WHERE _user._userid = pf-g004.USER-ID:
        PUT UNFORMATTED
            USER-ID '|' _user._user-name '|' seguridad SKIP.
    END.
END.

