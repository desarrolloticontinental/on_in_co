	��VWʉN(7   �                                              �< 372800EFutf-8 MAIN O:\on_in_co\APLIC\nxtl\dcdocu_cl.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,NroPed character 0 0,CodVen character 1 0,CodCli character 2 0,NomCli character 3 0,FmaPgo character 4 0,ImpTot decimal 5 0,FchPed date 6 0,RowNum integer 7 0,RowIdent character 8 0,RowMod character 9 0,RowIdentIdx character 10 0,RowUserProp character 11 0,ChangedFields character 12 0        tA              �-             n� tA  �              ��              $B     +   4� �  W   Է `  X   4� d  Y   ��   [   ��   \   �� <  ]   ��    ^   � 0  `   ? L� "   iSO8859-1                                                                           �@    �                                      �                   ��                A  @    t   ��   T�              ��  �   <A      HA                                                         PROGRESS                                    
    
                    �              �                                                                                                     
  4         �          H  |1  �   �1     ��  i�&N43  M                     `          x      �   �  P      ,  
    
                    �             �                                                                                          P          
  \  b      �  
    
                  �  �             H                                                                                          b          
    t      �  
    
                  p  8             �                                                                                          t          
  �  �      0  
    
                    �             �                                                                                          �          
  `  �      �  
    
                  �  �             L                                                                                          �          
    �      �  
    
                  t  <  	           �                                                                                          �          
  �  �      4  
    
                     �  
           �                                                                                          �          
  d  �      �  
    
                  �  �             P                                                                                          �          
    �      �                         x  @             �                                                                                          �            �  �      8                        $  �             �                                                                                          �            h	  �      �  
    
                  �  �	             T	                                                                                          �          
  
        �	  
    
                  |	  D
              
                                                                                                    
  �
        <
  
    
                  (
  �
             �
                                                                                                    
  l  $      �
                        �
  �             X                                                                                          $              4      �                        �  H                                                                                                       4            �  ?      @                        ,  �             �                                                                                          ?                P      �                        �  p             \                                                                                          P            �         �       �  X  �9     �9  �  ��      8:         �             h4          l5      �              �       �  X  (@     H@  �  _v      �@         �         �    h:          �;      �                 P�                                               T�          �    L l�                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                             "                  "                  "                  "                                                            !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4             "     5                  6                  7                  8                  9                  :                  ;                  <                  =                  >                  ?                  @                  A                  B                  C                  D                  E                  F                  G                  H                  I                  J                  K                  L                  M                 N                                 �#  �#  �#  �#  �#          �#             $  $  $  4$  ($          <$             \$  d$  l$  t$                              x$  �$  �$  �$  �$                         �$  �$  �$  �$  �$                         �$  �$  �$  �$  �$                         �$  %  %  4%  $%                          <%  D%  P%  `%                              h%  p%  x%  �%  �%          �%             �%  �%  �%  �%  �%          �%             �%  �%  �%   &  �%          &              &  $&  ,&  4&                              8&  @&  H&  `&  T&                          d&  l&  t&  �&  �&                          �&  �&  �&  �&  �&          �&              �&  �&  �&  �&                               '  '  '  0'   '                          4'  <'  D'  T'  L'          X'              l'  t'  x'  �'  �'          �'              �'  �'  �'  �'  �'          �'             �'  �'  �'  (  �'                         (  (  (   (                              $(  ,(  4(  <(                              @(  L(  T(  `(                              d(  t(  |(  �(                              �(  �(  �(  �(                              �(  �(  �(  �(                              �(  �(  )  )                              )   )  8)  D)                              H)  X)  d)  t)                              x)  �)  �)  �)  �)                          �)  �)  �)  �)                              �)  �)  �)  *  �)                         *  *  $*  L*  8*                         P*  X*  `*  x*  l*                         |*  �*  �*  �*  �*                         �*  �*  �*  �*  �*                         �*  �*  +  ,+  +                         0+  8+  @+  X+  L+                         \+  d+  t+  �+  �+                         �+  �+  �+  �+  �+                         �+  �+  �+  �+  �+                          ,  ,  ,  8,  (,                         <,  D,  T,  t,  d,                         x,  �,  �,  �,  �,                         �,  �,  �,  �,                             �,  �,  �,  �,                             �,  �,   -  -                             -  -  -  $-                             (-  0-  4-  <-                             @-  H-  L-  X-                              \-  d-  |-  �-                              �-  �-  �-  �-  �-                          �-  �-  �-  �-  �-                          �-  �-  �-  .  �-                          .  .  .  <.  (.                          @.  H.  P.  p.  `.                          t.  |.  �.  �.  �.                          �.  �.  �.  �.  �.                          �.  �.  �.  �.                              �.  �.  �.   /                              /  /  /   /                              $/  ,/  4/  L/  @/                          P/  X/  `/  p/  l/                          t/  |/  �/  �/  �/                          �/  �/  �/  �/                              �/  �/  �/  �/                              �/   0  0  0                              0  $0  ,0  80                              <0  H0  P0  \0                              `0  l0  �0  �0                             �0  �0  �0  �0                             �0  �0  �0  �0                              �0  �0   1  1                              1  1   1  01                              41  <1  D1  L1                              P1  X1  h1  x1  p1                                                                     CodCia  999 Cia Cia 0   C�digo de Compa�ia  CodDiv  x(5)    Division    Division    00000   Ingrese el Codigo de Division   CodSed  x(5)    Sede        CodPed  x(3)    Codigo  Codigo      NroPed  X(9)    No. Pedido  Numero!Pedido       FchPed  99/99/99    Fecha   Fch.Pedido  today   FchVen  99/99/99    Fecha Vencimiento   Fecha!Vcmto.    TODAY   FchEnt  99/99/99    Fecha Entrega   TODAY   CodCli  x(11)   Codigo  Codigo      C�digo del Cliente  NomCli  x(60)   Nombre  Nombre      Nombre del Cliente  DirCli  x(60)   Direcci�n   Direcci�n       Direcci�n del Cliente   Sede    x(5)    Sede        LugEnt  x(60)   Entregar en Entregar en     LugEnt2 x(60)   Lugar de entrega    Lugar de entrega        RucCli  x(11)   Ruc Ruc     Registro Unico de Contribuyente (Cliente)   DniCli  x(8)    DNI     NroOrd  x(15)   Orden de Compra Orden de!Compra     CodAlm  x(3)    Almac�n Almac�n     C�digo de almac�n   CodMon  9   Moneda  Cod!mon 1   C�digo de moneda    TpoCmb  Z,ZZ9.9999  Tipo de cambio  T/Cambio    0   Tipo de cambio  Usuario x(15)   usuario usuario     UsrAnu  x(15)   UsrAnu      UsrMod  x(15)   UsrMod      UsrDscto    x(15)   UsrDscto        UsrAprobacion   x(15)   UsrAprobacion       FchCre  99/99/9999 HH:MM:SS.SSS FchCre  ?   FchAnu  99/99/9999 HH:MM:SS.SSS FchAnu  ?   FchMod  99/99/9999 HH:MM:SS.SSS FchMod  ?   FchDscto    99/99/9999 HH:MM:SS.SSS FchDscto    ?   FchAprobacion   99/99/99    FchAprobacion   ?   Observa X(50)   Observaciones   Observaciones       FlgIgv  Si/No   Incluido IGV    Si  ImpBrt  ->>>,>>>,>>9.99 Importe Bruto   Importe Bruto   0   ImpDto  ->>>,>>>,>>9.99 Importe Descuento   Importe Descuento   0   PorDto  ->>9.99 % Dscto.    % Dscto.    0   ImpExo  ->>>,>>>,>>9.99 Importe Exonerado   Importe Exonerado   0   ImpVta  ->>>,>>>,>>9.99 Valor Venta Valor venta 0   ImpIgv  ->>>,>>>,>>9.99 Importe I.G.V.  Importe I.G.V.  0   PorIgv  ->>9.99 % I.G.V.    % I.G.V.    0   ImpIsc  ->>>,>>>,>>9.99 Importe Isc Importe Isc 0   ImpFle  ->>>,>>>,>>9.99 Importe Flete   Importe Flete   0   ImpInt  ->>>,>>>,>>9.99 Intereses   Intereses   0   ImpTot  ->>>,>>>,>>9.99 Importe Total   Importe Total   0   SdoAct  ->>>,>>>,>>9.99 Importe Total   Importe Total   0   ImpCto  ->>>,>>>,>>9.99 ImpCto  ImpCto  0   PesMat  ->>>,>>>,>>9.99 Peso (kg)   0   MrgUti  ->>>,>>9.99 Margen (%)  0   CodOri  x(3)    CodOri      NroOri  x(15)   NroOri      FlgEst  x   Estado  P   FlgSit  x   Situaci�n   X   FchSit  99/99/9999 HH:MM:SS.SSS FchSit  ?   NroCard x(8)    Tarjeta         FmaPgo  X(8)    Condicion de ventas Condicion de!venta      CodRef  x(3)    Codigo  Codigo      NroRef  X(15)   No. Referencia  Numero!Referencia       CodDept X(3)    Departamento    Departamento        CodProv X(3)    Provincias  Provincias      CodDist X(3)    Distrito    Distrito        CodPos  x(3)    Postal      FlgEnv  Si/No   Env�o?  No  Cmpbnte x(3)    Comprobante FAC TpoLic  Si/No   Licitacion  Licitacion  no  CodVen  X(3)    Vendedor            Hora    X(5)    Hora    Hora    string(TIME,"HH:MM")    Libre_c01   x(60)   Libre_c01       Libre_c02   x(60)   Libre_c02       Libre_c03   x(60)   Libre_c03       Libre_c04   x(60)   Libre_c04       Libre_c05   x(60)   Libre_c05       Libre_d01   ->>>,>>>,>>9.99<<<  Libre_d01   0   Libre_d02   ->>>,>>>,>>9.99<<<  Libre_d02   0   Libre_f01   99/99/99    Libre_f01   ?   Libre_f02   99/99/99    Libre_f02   ?   Glosa   X(60)   Observaciones       CodTer  x(8)    CodTer      Importe >,>>>,>>9.99    Importe Importe 0   �  $ 5 F [�  ���N������ 00000   ���                �����                  PX�         FAC  strin       ��  �    �    �        �        �        �        �        �                                  �     i  i  i      i  i  i  i      i  i  i      i  i  i      i  i  i  i      i  i  i  i  i      i  i  i  i  i      i 	 i 
 i     	 	 	3 	 	 	
 	 	 	1 	2 	    .   5   <   C   J   Q   X   _   f   m   t   {   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �         !  (  /  6  =  D  K  R  Y  `  g  n  u  |  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �          )  3  =  G  Q  [  e  k  r                                                                                                                                     	                  
                                                                                       �7  �7  �7  �7  �7                         �7  �7  �7  �7  �7                         �7  8  8  8  8           8             48  <8  D8  T8  L8          X8             l8  t8  |8  �8  �8                         �8  �8  �8  �8  �8                         �8  �8  �8  9   9                         9  9  (9  09                             49  @9  H9  T9                              X9  `9  h9  p9                             t9  �9  �9  �9                             �9  �9  �9  �9                                                                          NroPed  X(9)    No. Pedido  Numero!Pedido       CodVen  X(3)    Vendedor            CodCli  x(11)   Codigo  Codigo      C�digo del Cliente  NomCli  x(60)   Nombre  Nombre      Nombre del Cliente  FmaPgo  X(8)    Condicion de ventas Condicion de!venta      ImpTot  ->>>,>>>,>>9.99 Importe Total   Importe Total   0   FchPed  99/99/99    Fecha   Fch.Pedido  today   RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  ���������      �            o                �                �     i     i     i    	 	 	 	    J   �  f   m   �  g  Q   o  v    �  �                                                                                                                                     	                  
                                                                                                         �=  �=  �=  >  >                         >   >  (>  8>  4>                         <>  D>  L>  \>  T>          `>             t>  |>  �>  �>  �>          �>             �>  �>  �>  �>  �>                         �>  �>   ?   ?  ?                         $?  ,?  8?  L?  @?                         T?  \?  h?  p?                             t?  �?  �?  �?                              �?  �?  �?  �?                             �?  �?  �?  �?                             �?  �?  �?  �?                              �?  @  @  $@                                                                          NroPed  X(9)    No. Pedido  Numero!Pedido       CodVen  X(3)    Vendedor            CodCli  x(11)   Codigo  Codigo      C�digo del Cliente  NomCli  x(60)   Nombre  Nombre      Nombre del Cliente  FmaPgo  X(8)    Condicion de ventas Condicion de!venta      ImpTot  ->>>,>>>,>>9.99 Importe Total   Importe Total   0   FchPed  99/99/99    Fecha   Fch.Pedido  today   RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  ���������      �            o                �                �     i     i     i    	 	 	 	    J   �  f   m   �  g  Q   o  v    �  �  �    ��                            ����                                 p�                    ��    undefined                                                               �       t�  �   l   ��  ��                    �����               LR                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                     3   ����       $      H  ���                       8      
                       � ߱        �  �   "   D       �     E          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   ̃                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  _  b  L              \�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  d  j  �              4�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                  l  m  p              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  o  r  p              l�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  t  v  �              |�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  x  {  �	              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  }  ~  H              ԙ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  �  �  T              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  �  �  T              <�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                  �  �  |              Б                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  �  �  |              `�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  �  �  |              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                  �  �  |              ܍                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �              TB                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              @I                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              �I                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              �J                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              h]                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `              �u                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d              (v                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �              �R                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              �n                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              |Z                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              �j                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              PN                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              $1                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                  �  �  @$              H)                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                  �  �  l%              �-                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                  �  �  d'              �                     O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                  �  �  h(              �!                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                  �  �  �)              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                  �    @+              <                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                      �,              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                      �-              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                      �/              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                      �0              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                     !  �1              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                  #  %  �2              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3     �      CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	 �      CHARACTER,  canNavigate �3      �3      (4    �      LOGICAL,    closeQuery  4      44      `4   
 �      LOGICAL,    columnProps @4      l4      �4    �      CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	 �      CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    �      CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 �      LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 	      LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	        CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
        CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7    *      LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7    2      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7    ?      CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8    K      CHARACTER,  hasForeignKeyChanged    88      d8      �8    b      LOGICAL,    openDataQuery   |8      �8      �8    w      LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	 �      LOGICAL,    prepareQuery    9      49      d9    �      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    �      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	 �      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	 �      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;    �      CHARACTER,  assignDBRow                             <  �;      ��                      <              �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                      L=              |�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                      �>              �e                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                      �?              �h                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                       A              $;                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                  !  "  PB              �;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                  $  %  PC              4;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                  '  (  PD              � ;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                  *  +  PE              H,;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                  -  .  PF              8-;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                  0  2  \G              t0;                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  4  5  �H              �$;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  7  9  �I              `8;                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  ;  <  �J              �H;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  >  ?  �K              �I;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  A  D  �L              @(;                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M    �      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N          LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO          CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O    1      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P    @      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP    O      CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P    d      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q     p      CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !  }      CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "  �      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  �      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  �      CHARACTER,  getForeignValues    @R      lR      �R  %  �      CHARACTER,  getQueryPosition    �R      �R      �R  &  �      CHARACTER,  getQuerySort    �R      �R      S  '  �      CHARACTER,  getQueryString  �R      (S      XS  (  �      CHARACTER,  getQueryWhere   8S      dS      �S  )  �      CHARACTER,  getTargetProcedure  tS      �S      �S  *        HANDLE, indexInformation    �S      �S      T  +        CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,  +      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -  <      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .  K      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /  \      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0  j      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1  y      CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2  �      LOGICAL,    removeQuerySelection    �W      �W      (X  3  �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  �      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
 �      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7  �      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8  �      LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :        LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;        CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              L�;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\              ��;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                  �  �  �]              `�;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                  �  �  �^              @�;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                  �  �  `              h�;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                  �  �  a              P�;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                  �  �  b              $�;                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                  �  �  Dc              �;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                  �  �  Hd              �x;                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <  $      CHARACTER,  getASBound  �d      e      0e  = 
 2      LOGICAL,    getAsDivision   e      <e      le  >  =      CHARACTER,  getASHandle Le      xe      �e  ?  K      HANDLE, getASHasStarted �e      �e      �e  @  W      LOGICAL,    getASInfo   �e      �e      f  A 	 g      CHARACTER,  getASInitializeOnRun    �e       f      Xf  B  q      LOGICAL,    getASUsePrompt  8f      df      �f  C  �      LOGICAL,    getServerFileName   tf      �f      �f  D  �      CHARACTER,  getServerOperatingMode  �f      �f      g  E  �      CHARACTER,  runServerProcedure  �f      $g      Xg  F  �      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G  �      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K        LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L        LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M  '      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N  9      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              d�;                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl              ��;                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              t=<                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o              (E<                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p              �s;                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              �I<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              �L<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              �O<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t              �V<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u              \W<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                  �  �  �v              X<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                  �  �  �w              �^<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                  �  �  �x              `<                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                  �  �  |z              8p<                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                  �  �  �{              X<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                  �  �  �|              �<                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                      x~              �y<                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                      �              ��<                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                  
    ��               �<                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                      L�              �<                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                      t�              ��<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
 �      LOGICAL,    assignLinkProperty  ؃      �      8�  P  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  Q  �      CHARACTER,  getChildDataKey ��      ̄      ��  R  �      CHARACTER,  getContainerHandle  ܄      �      <�  S  �      HANDLE, getContainerHidden  �      D�      x�  T  �      LOGICAL,    getContainerSource  X�      ��      ��  U   	      HANDLE, getContainerSourceEvents    ��      ��      ��  V  	      CHARACTER,  getContainerType    ܅      �      <�  W  ,	      CHARACTER,  getDataLinksEnabled �      H�      |�  X  =	      LOGICAL,    getDataSource   \�      ��      ��  Y  Q	      HANDLE, getDataSourceEvents ��      ��      �  Z  _	      CHARACTER,  getDataSourceNames  Ԇ       �      4�  [  s	      CHARACTER,  getDataTarget   �      @�      p�  \  �	      CHARACTER,  getDataTargetEvents P�      |�      ��  ]  �	      CHARACTER,  getDBAware  ��      ��      �  ^ 
 �	      LOGICAL,    getDesignDataObject ȇ      �      (�  _  �	      CHARACTER,  getDynamicObject    �      4�      h�  `  �	      LOGICAL,    getInstanceProperties   H�      t�      ��  a  �	      CHARACTER,  getLogicalObjectName    ��      ��      ��  b  �	      CHARACTER,  getLogicalVersion   Ј      ��      0�  c  
      CHARACTER,  getObjectHidden �      <�      l�  d  
      LOGICAL,    getObjectInitialized    L�      x�      ��  e  %
      LOGICAL,    getObjectName   ��      ��      �  f  :
      CHARACTER,  getObjectPage   ̉      ��      (�  g  H
      INTEGER,    getObjectParent �      4�      d�  h  V
      HANDLE, getObjectVersion    D�      l�      ��  i  f
      CHARACTER,  getObjectVersionNumber  ��      ��      �  j  w
      CHARACTER,  getParentDataKey    Ċ      ��      $�  k  �
      CHARACTER,  getPassThroughLinks �      0�      d�  l  �
      CHARACTER,  getPhysicalObjectName   D�      p�      ��  m  �
      CHARACTER,  getPhysicalVersion  ��      ��      �  n  �
      CHARACTER,  getPropertyDialog   ȋ      �      (�  o  �
      CHARACTER,  getQueryObject  �      4�      d�  p  �
      LOGICAL,    getRunAttribute D�      p�      ��  q  �
      CHARACTER,  getSupportedLinks   ��      ��      ��  r        CHARACTER,  getTranslatableProperties   ��      �      (�  s        CHARACTER,  getUIBMode  �      4�      `�  t 
 9      CHARACTER,  getUserProperty @�      l�      ��  u  D      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    |�      č      ��  v  T      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ܍      $�      P�  w  i      CHARACTER,INPUT pcLink CHARACTER    linkProperty    0�      t�      ��  x  u      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      ��      �  y  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      x�      ��  z  �      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ̏      ��  {  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ܏      $�      T�  |  �      CHARACTER,  setChildDataKey 4�      `�      ��  }  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  p�      ��      �  ~  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ̐      �      @�    �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents     �      `�      ��  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled |�      ��      ��  �        LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ԑ      �      L�  �        LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ,�      l�      ��  �  )      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      Ȓ      ��  �  =      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ܒ      $�      T�  �  P      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 4�      x�      ��  �  ^      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Г      ��  � 
 r      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ܓ      �      P�  �  }      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    0�      x�      ��  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      Ȕ       �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      $�      \�  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   <�      x�      ��  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Е       �  �  �      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��       �      P�  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion    0�      p�      ��  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ̖       �  �        LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      (�      \�  �        LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   <�      |�      ��  �  3      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ԗ      �  �  I      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      ,�      \�  �  \      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   <�      ��      ��  �  l      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ܘ      �  �  ~      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      <�      h�  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	 �      CHARACTER,INPUT pcName CHARACTER    l�    *  ��  0�      �       4   �����                 @�                      ��                  +  X                  L$<                       +  Ě        ,  \�  ؛      �       4   �����                 �                      ��                  -  W                  �$<                       -  l�  �    D  �  ��      �       4   �����                 ��                      ��                  P  R                  \(<                       P  �         Q                                  ,     
                    � ߱        �  $  T  ��  ���                           $  V  @�  ���                       x                         � ߱        x�    \  ��  �      �      4   �����                �                      ��                  ]  !	                  )<                       ]  ��  H�  o   `      ,                                 ��  $   a  t�  ���                       �  @         �              � ߱        ��  �   b        Ȟ  �   c  �      ܞ  �   e        �  �   g  x      �  �   i  �      �  �   k  `      ,�  �   l  �      @�  �   m        T�  �   p  �      h�  �   r         |�  �   s  |      ��  �   u  �      ��  �   v  t      ��  �   w  �      ̟  �   x  ,      ��  �   y  �      ��  �     �      �  �   �  P	      �  �   �  �	      0�  �   �   
      D�  �   �  t
      X�  �   �  �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  H	  v	  ��              ��;                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        h�  $ \	  آ  ���                           O   t	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ  �                     8�    �	  ��  �      t      4   ����t                 �                      ��                  �	  
                  �<                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    M
  T�  Ц      x      4   ����x                �                      ��                  N
  �
                  l�;                       N
  d�  ��  �   P
  �      �  �   Q
  T      �  �   R
  �      0�  �   S
  D      D�  �   T
  �      X�  �   U
  �      l�  �   W
  p      ��  �   X
  �      ��  �   Y
  X      ��  �   Z
  �      ��  �   [
  �      Ч  �   \
  D       �  �   ]
  �       ��  �   ^
  �       �  �   _
  x!       �  �   `
  �!      4�  �   a
  h"      H�  �   b
  �"      \�  �   c
  `#      p�  �   d
  �#      ��  �   e
  X$      ��  �   f
  �$      ��  �   g
  �$      ��  �   h
  L%      Ԩ  �   i
  �%      �  �   j
  <&      ��  �   k
  �&      �  �   l
  4'      $�  �   m
  �'      8�  �   n
  ,(      L�  �   o
  h(      `�  �   q
  �(      t�  �   r
  X)      ��  �   s
  �)      ��  �   t
  *      ��  �   u
  �*      ĩ  �   v
  �*      ة  �   w
  l+      �  �   x
  �+       �  �   y
  \,      �  �   z
  �,      (�  �   {
  L-      <�  �   |
  �-      P�  �   }
  <.      d�  �   ~
  �.      x�  �   
  4/      ��  �   �
  �/          �   �
  $0      d�    �
  ��  8�      T0      4   ����T0                H�                      ��                  �
  �                  �<                       �
  ̪  \�  �   �
  �0      p�  �      (1      ��  �     �1      ��  �     2      ��  �     �2      ��  �     3      ԫ  �     |3      �  �     �3      ��  �     t4      �  �     �4      $�  �   	  l5      8�  �   
  �5      L�  �     d6      `�  �     �6      t�  �     L7      ��  �     �7      ��  �     <8      ��  �     �8      Ĭ  �     ,9      ج  �     �9      �  �     :       �  �     X:      �  �     �:      (�  �     H;      <�  �     �;      P�  �     8<          �     �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  9                  ��;                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �    �  ��  ��      K      4   ����K      /   �  а     �                          3   ����K             �                      3   ����<K  Զ    �  ,�  ��  �  XK      4   ����XK  
              ��                      ��             
     �  [                  h�;                       �  <�  ̱  �   �  �K      $�  $  �  ��  ���                       �K     
                    � ߱        8�  �   �  L      ��  $     d�  ���                       ,L  @         L              � ߱        L�  $    ��  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱        ܳ  V     �  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        l�  $  *  x�  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   <  �  ���                                      ̵                      ��                  ]  �                  ��;                       ]  ��  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   r  �  ���                        adm-clone-props �  ��              �     W     `                          \  �                     start-super-proc    �  d�  �           �     X                                  �                     l�      �   �      �X      4   �����X      /     ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $  -  ��  ���                       @Y                         � ߱        ��    =  �  \�  ��  \Y      4   ����\Y                и                      ��                  >  B                  �<                       >  �  pY                     �Y                     �Y                         � ߱            $  ?  l�  ���                             C  �  T�      �Y      4   �����Y  �Y                         � ߱            $  D  (�  ���                       �Y                         � ߱        ع  $  H  ��  ���                       Ժ    K  ��  �  \�  �Y      4   �����Y      $  L  0�  ���                       Z                         � ߱            �   i  ,Z      lZ     
                �Z                     8\  @        
 �[              � ߱         �  V   }  p�  ���                        �  �   �  D\      �    /  0�  @�      �\      4   �����\      /   0  l�     |�                          3   �����\            ��                      3   �����\  �\     
                P]                     �^  @        
 `^              � ߱        ��  V   <  ��  ���                        �^     
                h_                     �`  @        
 x`              � ߱        ̼  V   `  <�  ���                        L�    �  �  d�      �`      4   �����`                t�                      ��                  �  �                  �<                       �  ��  �  /   �  ��     ��                          3   �����`            н                      3   �����`      /   �  �     �                          3   ����a            <�                      3   ����8a  ��  /  M  x�         la                      3   ����Ta  initProps   x�  ��              ,     Y     $                                	                                   ̿          t�  \�      ��                 %  >  ��              ��<                    O   ����    e�          O   ����    R�          O   ����    ��      
                      ��          ��  p   0  �t  �      ;  t�  d�     �t                                        ��                  1  M                  \�<                       1  ��   �  ��     �t                                        ��                  N  j                  ��<                       N  ��  ��  |�     �t                                        ��                  k  �                  ��<                       k  �  �  �     u                                        ��                  �  �                  ��<                       �  ��  ��  ��     u                                        ��                  �  �                  h�<                       �  (�  0�   �     0u                                        ��                  �  �                  8�<                       �  ��  ��  ��     Du                                        ��                  �  �                  �<                       �  @�  H�  8�     Xu                                        ��                  �                    ��<                       �  ��  ��  ��     lu  	                                      ��             	       5                  ��<                         X�  `�  P�     �u  
                                      ��             
     6  R                  ��<                       6  ��  ��  ��     �u                                        ��                  S  o                  ��<                       S  p�  x�  h�     �u                                        ��                  p  �                  \ =                       p  ��  �  ��     �u                                        ��                  �  �                  ,=                       �  ��  ��  ��     �u                                        ��                  �  �                  8=                       �  �  �  �     �u                                        ��                  �  �                  =                       �  ��  ��  ��     �u                                        ��                  �                     �=                       �  ,�  4�  $�     v                                        ��                                      �=                         ��      ��      v                                        ��                    :                  �=                         D�      O   =  ��  ��  4v               D�          ,�  8�   , �                                                       �     ��                            ����                            ��  d�  ��  �      ��     Z     L�                      � H�                       ��    S  �  ��      @v      4   ����@v                ��                      ��                  T  h                   =                       T  �  ��  /   U  ��     ��                          3   ����Pv            ��                      3   ����pv  h�  /   V  (�     8�                          3   �����v            X�                      3   �����v  ��  /   [  ��     ��                          3   �����v            ��                      3   �����v      /   a   �     �                          3   ����w            0�                      3   ����$w  Dw     
                �w                     y  @        
 �x              � ߱        ��  V   �  @�  ���                        ��  $  �  ��  ���                       $y                         � ߱        @y     
                �y                     {  @        
 �z              � ߱        ��  V   �  (�  ���                        t�  $  �  ��  ���                       {     
                    � ߱        ,{     
                �{                     �|  @        
 �|              � ߱        ��  V     �  ���                        \�  $    ��  ���                       }     
                    � ߱        }     
                �}                     �~  @        
 �~              � ߱        ��  V   &  ��  ���                        D�  $  @  ��  ���                       �~                         � ߱        $     
                �                     ��  @        
 ��              � ߱        p�  V   J  ��  ���                        ��  �   d  �      @�  $  e  ��  ���                       (�     
                    � ߱        <�     
                ��                     �  @        
 Ȃ              � ߱        l�  V   o  ��  ���                        ��  $  �  ��  ���                       �     
                    � ߱        ��  �   �  (�      0�  $  �  �  ���                       h�     
                    � ߱        D�  �   �  |�      ��  $  �  p�  ���                       ��                         � ߱              �  ��  ��      ؃      4   ����؃      /   �  ��     �                          3   ������  4�     
   $�                      3   �����  d�        T�                      3   ���� �  ��        ��                      3   ����4�            ��                      3   ����P�  pushRowObjUpdTable  ��  ��  �                   [      �                               7                     pushTableAndValidate    ��  4�  �           |     \     �                          �  T                     remoteCommit    L�  ��  �           p     ]     �                          �  �                     serverCommit    ��  �  �           l     ^     �                          �  �                                     4�          �  ��      ��                    %  �              `H                    O   ����    e�          O   ����    R�          O   ����    ��          O   #  ��  ��  ��    ��                            ����                            $�  P�      ��              _      L�                      
�     �                     disable_UI  ��  ��                      `      �                               �  
                    �  �    ����  �       ��       ��  8   ����   ��  8   ����       8   ����       8   ����       ��  ��      viewObject  ,   ��  ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  �  4�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  �  x�  ��      returnFocus ,INPUT hTarget HANDLE   h�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  ��  �      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  \�  l�      removeAllLinks  ,   L�  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE p�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  t�  ��      hideObject  ,   d�  ��  ��      exitObject  ,   ��  ��  ��      editInstanceProperties  ,   ��  ��  ��      displayLinks    ,   ��  �  �      createControls  ,   ��  (�  8�      changeCursor    ,INPUT pcCursor CHARACTER   �  d�  p�      applyEntry  ,INPUT pcField CHARACTER    T�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  �  �      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  h�  p�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE X�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  ��  �      runServerObject ,INPUT phAppService HANDLE  ��  8�  L�      disconnectObject    ,   (�  `�  p�      destroyObject   ,   P�  ��  ��      bindServer  ,   t�  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  ��  �      startFilter ,   ��  �  (�      releaseDBRow    ,   �  <�  L�      refetchDBRow    ,INPUT phRowObjUpd HANDLE   ,�  x�  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE h�  ��  ��      fetchDBRowForUpdate ,   ��  ��  ��      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL ��  (�  8�      compareDBRow    ,   �  L�  `�      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   <�  ��  ��      assignDBRow ,INPUT phRowObjUpd HANDLE   ��  �  �      updateState ,INPUT pcState CHARACTER    ��  D�  X�      updateQueryPosition ,   4�  l�  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    \�  ��  ��      undoTransaction ,   ��  ��  ��      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  ��  x�  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   h�  ��  �      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   ��  P�  d�      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  @�  ��  ��      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  ��  ,�  @�      startServerObject   ,   �  T�  d�      setPropertyList ,INPUT pcProperties CHARACTER   D�  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��  ��  ��      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    ��  ��  ��      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  ��   �      rowObjectState  ,INPUT pcState CHARACTER    ��  ,�  <�      retrieveFilter  ,   �  P�  d�      restartServerObject ,   @�  x�  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   h�  ��  ��      refreshRow  ,   p�  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  �  $�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  �  T�  l�      initializeServerObject  ,   D�  ��  ��      initializeObject    ,   p�  ��  ��      home    ,   ��  ��  ��      genContextList  ,OUTPUT pcContext CHARACTER ��   �  �      fetchPrev   ,   ��   �  ,�      fetchNext   ,   �  @�  L�      fetchLast   ,   0�  `�  l�      fetchFirst  ,   P�  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   p�  ��  ��      endClientDataRequest    ,   ��  ��  ��      destroyServerObject ,   ��  �  �      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    ��  h�  x�      dataAvailable   ,INPUT pcRelative CHARACTER X�  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  ��  �      commitTransaction   ,   ��   �  0�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    �  ��  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 %     adecomm/as-utils.w 
"   
   �    }        �
"     
    �     }        �� z  >   %               � 
" 	   
 �%              h �P  \         (          
�                          
�            � �   N
" 	   
 3
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 3�               1� �  
 3� �   �%               o%   o           � �    3
"   
 3�           �    1� �   3� �   �%               o%   o           � �   3
"   
 3�           �    1� �  
 3� �   �%               o%   o           � 
   3
"   
 3�           l    1�    3� �   �%               o%   o           � �    3
"   
 3�           �    1� (   3� �   �%               o%   o           � 7   3
"   
 3�           T    1� L   3� X   �%               o%   o           %               
"   
 ��          �    1� `   �� p     
"   
 3�               1� w   3� �   �%               o%   o           � �  3
"   
 3�           �    1� �   3� �   �%               o%   o           � �  S 3
"   
 3�           �    1� �   3� X   �%               o%   o           %               
"   
 3�           p    1� �   3� X   �%               o%   o           %               
"   
 3�           �    1�    3� X   �%               o%   o           %              
"   
 ��          h    1�    �� X     
"   
 3�           �    1� -  
 3� X   �%               o%   o           %               
"   
 3�                1� 8   3� �   �%               o%   o           � �    3
"   
 ��          �    1� @   �� p     
"   
 3�           �    1� P   3� �   �%               o%   o           � f  t 3
"   
 ��          D	    1� �  
 �� p     
"   
 3�           �	    1� �   3� �   �%               o%   o           � �  � 3
"   
 3�           �	    1� �   3� �   �%               o%   o           � �    3
"   
 3�           h
    1� �  
 3� �   �%               o%   o           %               
"   
 ��           �
    1� �   �� X   �%               o%   o           %              
"   
 ��           `    1� �   �� �   �%               o%   o           � �    �
"   
 ��           �    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           P    1� �  
 �� �   �%               o%   o           � �    �
"   
 ��           �    1� �   �� �  	 �%               o%   o           � �  / �
"   
 ��          8    1� )   �� �  	   
"   
 ��           t    1� ;   �� �  	 �o%   o           o%   o           � �    �
"   
 ��          �    1� N   �� �  	   
"   
 ��           $    1� ]   �� �  	 �o%   o           o%   o           � �    �
"   
 ��          �    1� m   �� X     
"   
 ��          �    1� {   �� �  	   
"   
 ��              1� �   �� �  	   
"   
 ��          L    1� �   �� �  	   
"   
 ��           �    1� �   �� X   �o%   o           o%   o           %              
"   
 ��              1� �   �� �  	   
"   
 ��          @    1� �  
 �� �     
"   
 ��          |    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          0    1�    �� �  	   
"   
 ��          l    1�   	 �� �  	   
"   
 ��          �    1� %   �� �  	   
"   
 ��          �    1� 8   �� �  	   
"   
 ��                1� O   �� �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 <
"   
   
"   
 N(�  L ( l       �        �    �� [   � P   �        �    �@    
� @  , 
�            �� d     p�               �L
�    %              � 8          � $         � k          
�    � �     
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 3�           �    1� �  
 3� �   �%               o%   o           � �    3
"   
 3�           <    1� �  
 3� �   �%               o%   o           o%   o           
"   
 3�           �    1� �   3� p   �%               o%   o           o%   o           
"   
 3�           4    1� �   3� X   �%               o%   o           %               
"   
 3�           �    1� �   3� X   �%               o%   o           %               
"   
 3�           ,    1� �   3� �   �%               o%   o           � �    3
"   
 3�           �    1� �   3� X   �%               o%   o           %              
"   
 3�               1� �   3� X   �%               o%   o           o%   o           
"   
 3�           �    1� �   3� �   �%               o%   o           o%   o           
"   
 3�               1� �  	 3� �   �%               o%   o           � �    3
"   
 3�           �    1�     3� �   �%               o%   o           o%   o           
"   
 3�               1�    3� �   �%               o%   o           o%   o           
"   
 3�           �    1� #   3� X   �%               o%   o           %               
"   
 3�           �    1� 3   3� X   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 ��           �    1� ?  
 �� X   �%               o%   o           %              
"   
 ��           H    1� J   �� �   �%               o%   o           o%   o           
"   
 ��           �    1� V   �� �   �%               o%   o           � �    �
"   
 ��           8    1� d   �� �   �%               o%   o           o%   o           
"   
 ��          �    1� p   �� p     
"   
 ��           �    1� }   �� �   �%               o%   o           � �  ! �
"   
 ��           d    1� �   �� �   �%               o%   o           � �    �
"   
 ��           �    1� �   �� �   �%               o%   o           � �   �
"   
 ��          L    1� �   �� �     
"   
 ��          �    1� �   �� p     
"   
 ��           �    1�    �� �   �%               o%   o           � �    �
"   
 ��          8     1�   
 �� p     
"   
 ��           t     1�    �� X   �%               o%   o           o%   o           
"   
 ��           �     1� -   �� X   �%               o%   o           %               
"   
 ��           l!    1� :   �� X   �%               o%   o           %               
"   
 ��           �!    1� K   �� �   �%               o%   o           � �    �
"   
 ��           \"    1� [   �� �   �%               o%   o           o%   o           
"   
 ��           �"    1� g   �� X   �%               o%   o           %              
"   
 ��           T#    1� x   �� X   �%               o%   o           %               
"   
 ��           �#    1� �   �� X   �%               o%   o           %               
"   
 ��          L$    1� �   �� p     
"   
 ��          �$    1� �   �� �     
"   
 ��           �$    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           @%    1� �   �� �   �%               o%   o           � �    �
"   
 ��           �%    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           0&    1� �   �� X   �o%   o           o%   o           o%   o           
"   
 ��           �&    1� �   �� �  	 �%               o%   o           o%   o           
"   
 ��           ('    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           �'    1� 
  
 �� �   �%               o%   o           o%   o           
"   
 ��           (    1�    �� �     
"   
 ��           \(    1� &   �� �   �%               o%   o           � =  4 �
"   
 ��           �(    1� r  
 �� X   �%               o%   o           %              
"   
 ��          L)    1� }   �� p     
"   
 ��           �)    1� �   �� �   �%               o%   o           � �    �
"   
 ��           �)    1� �   �� X   �%               o%   o           %              
"   
 ��           x*    1� �   �� �   �%               o%   o           � �    �
"   
 ��           �*    1� �   �� �   �%               o%   o           � �    �
"   
 ��           `+    1� �   �� �   �%               o%   o           � �    �
"   
 ��           �+    1� �   �� X   �%               o%   o           %               
"   
 ��           P,    1� �  	 �� p   �%               o%   o           o%   o           
"   
 ��           �,    1� �   �� �   �%               o%   o           � �  	 �
"   
 ��           @-    1�    �� �   �%               o%   o           %       �       
"   
 ��           �-    1�    �� �   �%               o%   o           � �    �
"   
 ��           0.    1�    �� X   �o%   o           o%   o           %              
"   
 ��           �.    1� )   �� X   �%               o%   o           %               
"   
 ��           (/    1� @   �� �   �%               o%   o           o%   o           
"   
 ��           �/    1� Q   �� �  	 �%               o%   o           � �    �
"   
 ��          0    1� b   �� �  	   P �L 
�H T   %              �     }        �GG %              
"   
 3�           �0    1� o  
 3� �   �%               o%   o           � �    3
"   
 3�           1    1� z   3� X   �%               o%   o           %               
"   
 3�           �1    1� �  	 3� �   �%               o%   o           � �    3
"   
 3�           2    1� �   3� �   �%               o%   o           � �    3
"   
 3�           �2    1� �   3� X   �%               o%   o           %               
"   
 3�           �2    1� �   3� �   �%               o%   o           � �    3
"   
 3�           p3    1� �   3� �   �%               o%   o           o%   o           
"   
 3�           �3    1� �   3� �   �%               o%   o           o%   o           
"   
 3�           h4    1� �   3� X   �%               o%   o           o%   o           
"   
 3�           �4    1� �   3� X   �%               o%   o           o%   o           
"   
 3�           `5    1� �   3� X   �%               o%   o           o%   o           
"   
 3�           �5    1�    3� �   �%               o%   o           o%   o           
"   
 3�           X6    1�   	 3� �  	 �%               o%   o           � �    3
"   
 3�           �6    1�   
 3� �  	 �%               o%   o           � �    3
"   
 3�           @7    1� *   3� �   �%               o%   o           � �    3
"   
 3�           �7    1� 9   3� �   �%               o%   o           o%   o           
"   
 3�           08    1� G   3� �   �%               o%   o           o%   o           
"   
 3�           �8    1� T   3� �   �%               o%   o           � �    3
"   
 3�            9    1� i   3� �   �%               o%   o           � �    3
"   
 3�           �9    1� x   3� �  	 �%               o%   o           o%   o           
"   
 ��          :    1� �   �� p     
"   
 3�           L:    1� �   3� �   �%               o%   o           � �    3
"   
 3�           �:    1� �   3� �   �%               o%   o           o%   o           
"   
 3�           <;    1� �   3� X   �%               o%   o           o%   o           
"   
 3�           �;    1� �  
 3� �   �%               o%   o           � �    3
"   
 3�           ,<    1� �   3� �   �%               o%   o           � �    3
"   
 3�           �<    1� �   3� X   �%               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 ��           p=    1� �  	 �� p   �%               o%   o           o%   o           
"   
 ��           �=    1�    �� p   �%               o%   o           o%   o           
"   
 ��           h>    1�    �� p   �%               o%   o           o%   o           
"   
 ��           �>    1� %   �� X   �%               o%   o           %              
"   
 ��           `?    1� 9   �� �   �%               o%   o           � R  M �
"   
 ��           �?    1� �   �� X   �%               o%   o           %              
"   
 ��           P@    1� �   �� X   �%               o%   o           %               
"   
 ��           �@    1� �   �� X   �%               o%   o           %               
"   
 ��           HA    1� �   �� �  	 �%               o%   o           � �   �
"   
 ��           �A    1� �   �� X   �%               o%   o           %               
"   
 ��           8B    1� 	   �� �  	 �%               o%   o           o%   o           
"   
 ��           �B    1�    �� X   �o%   o           o%   o           %              
"   
 ��           0C    1� &   �� �  	 �o%   o           o%   o           � �    �
"   
 ��           �C    1� 9   �� p   �o%   o           o%   o           o%   o           
"   
 ��            D    1� I   �� p   �o%   o           o%   o           o%   o           
"   
 ��           �D    1� Y   �� �  	 �o%   o           o%   o           o%   o           
"   
 ��           E    1� i   �� p   �o%   o           o%   o           o%   o           
"   
 ��           �E    1� x   �� �  	 �o%   o           o%   o           � �   �
"   
 ��           F    1� �   �� �  	 �o%   o           o%   o           � �   �
"   
 ��           |F    1� �   �� X   �%               o%   o           %               
"   
 ��           �F    1� �   �� X   �%               o%   o           %               
"   
 ��          tG    1� �   �� �  	   
"   
 ��           �G    1� �   �� X   �%               o%   o           %               
"   
 ��           ,H    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           �H    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           $I    1�    �� X   �%               o%   o           o%   o           
"   
 ��           �I    1� %   �� �   �%               o%   o           � �    �
"   
 ��           J    1� 4   �� B   �%               o%   o           %               
"   
 ��           �J    1� J  	 �� X   �%               o%   o           %                "    �%     start-super-proc ��%     adm2/smart.p �NP �L 
�H T   %              �     }        �GG %              
"   
   �       �K    6� [     
"   
   
�        �K    8
"   
   �        �K    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 N
"   
 �
"   
 N
"   
   (�  L ( l       �        �M    �� [   � P   �        �M    �@    
� @  , 
�       �M    �� d   Np�               �L
�    %              � 8       N    � $         � k          
�    � �   N
"   
 �p� @  , 
�       O    �� w   �p�               �L"  	  , �   � �   3� �   ��     }        �A      |    "  	    � �   <%              (<   \ (    |    �     }        �A� �   �A"  
  3    "  	  N"  
  3  < "  	  N"  
  3(    |    �     }        �A� �   �A"  
  3
�H T   %              �     }        �GG %              
"   
 N
"   
 �
"   
 N
"   
   (�  L ( l       �        �P    �� [   � P   �        �P    �@    
� @  , 
�       �P    �� d   Np�               �L
�    %              � 8      Q    � $         � k          
�    � �   N
"   
 �p� @  , 
�       R    �� �  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 <
"   
   
"   
   (�  L ( l       �        �R    �� [   � P   �        �R    �@    
� @  , 
�       �R    �� d     p�               �L
�    %              � 8      �R    � $         � k          
�    � �     
"   
 �p� @  , 
�       �S    �� �  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�       XT    ��      p�               �L%               
"   
  p� @  , 
�       �T    �� ]    p�               �L%               
"   
  p� @  , 
�       U    �� ;    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
  (   � 
"   
 N    �        �U    �� [   �
"   
   � 8      DV    � $         � k          
�    � �   N
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6� [     
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    � �   <
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 N    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 "      �       }        �
"   
 �%              %                "    �%     start-super-proc ��%     adm2/appserver.p !<�    � .     
�    �     }        �%               %      Server  - �     }        �    "    ;� �    �%               %      Client      "    ;� �    �%      NONE    p�,  8         $     "    <        � O   N
�    
�H T   %              �     }        �GG %              
"   
 N
"   
 �
"   
 N
"   
   (�  L ( l       �        �Z    �� [   � P   �        �Z    �@    
� @  , 
�       �Z    �� d   Np�               �L
�    %              � 8      �Z    � $         � k          
�    � �   N
"   
 �p� @  , 
�       �[    ��     �p�               �L"    , p�,  8         $     "    <        � ]   N
�     "    �%     start-super-proc ��%     adm2/dataquery.p W;
�H T   %              �     }        �GG %              
"   
 N
"   
 �
"   
 N
"   
 N(�  L ( l       �         ]    �� [   � P   �        ,]    �@    
� @  , 
�       8]    �� d   Np�               �L
�    %              � 8      D]    � $         � k   N     
�    � �   N
"   
 �p� @  , 
�       T^    �� �   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 N
"   
 �
"   
 N
"   
 N(�  L ( l       �        8_    �� [   � P   �        D_    �@    
� @  , 
�       P_    �� d   Np�               �L
�    %              � 8      \_    � $         � k   N     
�    � �   N
"   
 �p� @  , 
�       l`    �� L   �p�               �L%               "    �%     start-super-proc ��%     adm2/query.p �N%     start-super-proc ��%     adm2/queryext.p % 	    initProps N
�    %4 ) $   FOR EACH CDocu NO-LOCK INDEXED-REPOSITION ;�   � �     � �     �       
�H T   %              �     }        �GG %              
"   
 N
"   
 �
"   
 N
"   
   (�  L ( l       �        $b    �� [   � P   �        0b    �@    
� @  , 
�       <b    �� d   Np�               �L
�    %              � 8      Hb    � $         � k          
�    � �   N
"   
 �p� @  , 
�       Xc    ��    �p�               �L"    ,     "    <� �    �
�H T   %              �     }        �GG %              
"   
 N
"   
 �
"   
 N
"   
   (�  L ( l       �        d    �� [   � P   �        (d    �@    
� @  , 
�       4d    �� d   Np�               �L
�    %              � 8      @d    � $         � k          
�    � �   N
"   
 �p� @  , 
�       Pe    �� �  	 �p�               �L"    , %               �    "      � �         %              %                   "      %                  "      "      "     T(        "    ;%              "    ;� �   �"      �       "    N�    "    ;� �   �� �      � �   N�    "     � �    S    "      "    �    "    ;%                � @    �     t T     P   4       �"      (0       4       ;"      � �      � �    N� �   ;T ,  %              T   "    ;"    �� �     � �   N� �   ;T    �    "    ;� �   �"      � �   N"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "    N%              � �    �� �     4  <     "      
�H T   %              �     }        �GG %              
"   
 N
"   
 �
"   
 N
"   
   (�  L ( l       �        j    �� [   � P   �        $j    �@    
� @  , 
�       0j    �� d   Np�               �L
�    %              � 8      <j    � $         � k          
�    � �   N
"   
 �p� @  , 
�       Lk    �� o  
 �p�               �L"    ,       "  
  <�    � �  0 <� �   �      "  	    �    � �  0 �� �   <�   � �     � �     � �  0 N�   � �     � �   N� �  0 <�   � �     � �     � �  0   
�H T   %              �     }        �GG %              
"   
 �
"   
 N
"   
 �
"   
 �(�  L ( l       �        �l    �� [   � P   �        �l    �@    
� @  , 
�       �l    �� d   �p�               �L
�    %              � 8       m    � $         � k          
�    � �     
"   
 �p� @  , 
�       n    ��    �p�               �L"    , 
"   
   p� @  , 
�       hn    �� �     p�               �L"    , 
"   
  p� @  , 
�       �n    �� �    p�               �L"    ,     %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    � �  0   � �         "  	  <�     "    <T    "      "      @ A,    �   � �   �� �     "    N"       T      @   "    �(        "      � �    N� �      � �   N"         "  	   %              D H   @ A,    �   � �   N� �     "    N"    ;,    S   "    N� �  0 ;� �   �%                T      @   "    �(        "      � �    N� �      � �   N"    <     "  
   %                         "    �� �     "    N           "      � �   N"      
�H T   %              �     }        �GG %              
"   
 ;
"   
   
"   
 ;
"   
 N(�  L ( l       �        �r    �� [   � P   �        �r    �@    
� @  , 
�       �r    �� d   ;p�               �L
�    %              � 8       s    � $         � k   N     
�    � �   �
"   
 �p� @  , 
�       t    �� �   �p�               �L"    , 
"   
   p� @  , 
�       ht    �� �     p�               �L"    , "      %              %              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    �%     start-super-proc ��%     adm2/data.p %     start-super-proc ��%     adm2/dataext.p %     start-super-proc ��%     adm2/dataextcols.p %     start-super-proc ��%     adm2/dataextapi.p 
�H T   %              �     }        �GG %              
"   
 N
"   
 �
"   
 N
"   
 N(�  L ( l       �        �w    �� [   � P   �        �w    �@    
� @  , 
�       �w    �� d   Np�               �L
�    %              � 8      �w    � $         � k   N     
�    � �   N
"   
 �p� @  , 
�       �x    �� �   �p�               �L%               %     "nxtl/dcdocu.i" 
�H T   %              �     }        �GG %              
"   
 N
"   
 �
"   
 N
"   
   (�  L ( l       �        �y    �� [   � P   �        �y    �@    
� @  , 
�       �y    �� d   Np�               �L
�    %              � 8      �y    � $         � k          
�    � �   N
"   
 �p� @  , 
�       �z    �� �   �p�               �L"    , 
�     	         �G
�H T   %              �     }        �GG %              
"   
 N
"   
 �
"   
 N
"   
   (�  L ( l       �        x{    �� [   � P   �        �{    �@    
� @  , 
�       �{    �� d   Np�               �L
�    %              � 8      �{    � $         � k          
�    � �   N
"   
 �p� @  , 
�       �|    ��   
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 N
"   
 �
"   
 N
"   
   (�  L ( l       �        d}    �� [   � P   �        p}    �@    
� @  , 
�       |}    �� d   Np�               �L
�    %              � 8      �}    � $         � k          
�    � �   N
"   
 �p� @  , 
�       �~    �� �  	 �p�               �L
"   
 , 
"   
 �     � �  	   �        �~    �
�H T   %              �     }        �GG %              
"   
 N
"   
 �
"   
 N
"   
   (�  L ( l       �        p    �� [   � P   �        |    �@    
� @  , 
�       �    �� d   Np�               �L
�    %              � 8      �    � $         � k          
�    � �   N
"   
 �p� @  , 
�       ��    �� K   �p�               �L"    , 
"   
   �       ��    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 N
"   
 �
"   
 N
"   
   (�  L ( l       �        ��    �� [   � P   �        ��    �@    
� @  , 
�       ��    �� d   Np�               �L
�    %              � 8      ��    � $         � k          
�    � �   N
"   
 �p� @  , 
�       ��    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 <        � �   N
�    
�             �Gp�,  8         $     
"   
 <        � �   N
�    �    �      
�        "    <� �    �%     modifyListProperty 
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � �     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           �   l       ��                 j  �  �               �;                    O   ����    e�          O   ����    R�          O   ����    ��        $  y  �   ���                       �U     
                    � ߱              z  (  �      V      4   ����V                �                      ��                  {  �                  t�<                       {  8  �  �  |  PV            ~  �  `      �V      4   �����V                p                      ��                    �                  ؾ<                         �  �  o   �      ,                                 �  �   �  �V      �  �   �  �V      $  $  �  �  ���                        W     
                    � ߱        8  �   �  @W      L  �   �  `W      `  �   �  �W          $   �  �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               (�<                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  ��<                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �   �  �X          O   �  ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��                 Y    �               �<                    O   ����    e�          O   ����    R�          O   ����    ��        $  y  �   ���                       ta                         � ߱        �  $  z  8  ���                       �a                         � ߱        �a     
                Tb                     �c  @        
 dc              � ߱        �  V   �  d  ���                        �    �    �      �c      4   �����c  �c     
                Ld                     �e  @        
 \e              � ߱            V   �     ���                          $  $  �  ���                       �e                         � ߱        �  $  %  4  ���                       �e                         � ߱          �      4  8                      ��        0         '  =                  ��<      Tf     �     '  `      $  '    ���                       �e                         � ߱        �  $  '  `  ���                       f                         � ߱            4   ����4f  `f                     �f                     �f                     g                     $g                         � ߱        d  $  (  �  ���                             5  �  �      Dg      4   ����Dg      $  6  �  ���                       lg          �h             � ߱        �  $  @    ���                       �h                         � ߱          �        |                      ��        0         B  G                  ��<      8i     8     B  @      $  B  �  ���                       �h                         � ߱        l  $  B  @  ���                       �h                         � ߱            4   ����i      $  D  �  ���                       Li                         � ߱        �i     
                Hj                     �k  @        
 Xk              � ߱        �  V   R  �  ���                        �k       
       
       �k       	       	       l                     8l                         � ߱        	  $  �  d  ���                       
  $  =  <	  ���                       dl                         � ߱        �l     
                m                     \n  @        
 n          �n  @        
 tn          o  @        
 �n              � ߱        �
  V   I  h	  ���                          �
        |                      ��        0         �  �                  ��<      �o     T     �  0
      $  �  �
  ���                       o                         � ߱        \  $  �  0  ���                       Ho                         � ߱        l  4   ����po      4   �����o  �  $  �  �  ���                       p                         � ߱        �    �  �  l      0p      4   ����0p                �                      ��                  �  �                  P�<                       �     tp                     �p       	       	           � ߱            $  �  |  ���                             �    �      q      4   ����q                �                      ��                  �  �                  ��<                       �    �q                      r       
       
           � ߱            $  �  �  ���                       (r                     \r                         � ߱           $  �    ���                       �r     
                s                     \t  @        
 t          �t  @        
 tt              � ߱            V   �  �  ���                                    7 �          �  d  � `                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             �   �   �   �   �   �   �   �       $  4  D  T  d  t  �  \  �  �  �  �  8HXhx��������(8HX      �   �   �   �   �   �   �   �      $  4  D  T  d  t  �   \ �  �  �  �  8HXhx��������(8HX  �                    � �                     �          ��                            ����                                                        �   l       ��                  �  �  �               ̿                    O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   l       ��                  �  �  �               X�                    O   ����    e�          O   ����    R�          O   ����    ��      J       �              �                  $                  d  /  �  $     4  ��                      3   ����l�            T                      3   ������      O   �  ��  ��  ��               �          �  �    �                                             ��                            ����                                            H          �   l       ��                  �  �  �               0�                    O   ����    e�          O   ����    R�          O   ����    ��      i       �              �                $                  s       ,             �          ~                                �  /  �  t     �  Ą                      3   ������            �                      3   ����̄     /  �  �     �  �                      3   ����؄  x                             3   ������      $   �  L  ���                                                   � ߱                  �  �                  3   �����      $   �  �  ���                                                   � ߱        X  $  �  ,  ���                       �                         � ߱            O   �  ��  ��  0�               �          �  �   @ �                                                              0              0           ��                            ����                                                       �   l       ��                  �    �               �                    O   ����    e�          O   ����    R�          O   ����    ��      �       $                  s                    �          ~                      �              /    L     \  `�                      3   ����D�  �        |  �                  3   ����h�      $     �  ���                                                   � ߱                                      3   ����t�      $     @  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   l       ��                  h  s  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��            r  �   �       ��      4   ������      �   r  ��    ��                            ����                            TXS appSrvUtils CDocu Cabecera de COT PED O/D CodCia CodDiv CodSed CodPed NroPed FchPed FchVen FchEnt CodCli NomCli DirCli Sede LugEnt LugEnt2 RucCli DniCli NroOrd CodAlm CodMon TpoCmb Usuario UsrAnu UsrMod UsrDscto UsrAprobacion FchCre FchAnu FchMod FchDscto FchAprobacion Observa FlgIgv ImpBrt ImpDto PorDto ImpExo ImpVta ImpIgv PorIgv ImpIsc ImpFle ImpInt ImpTot SdoAct ImpCto PesMat MrgUti CodOri NroOri FlgEst FlgSit FchSit NroCard FmaPgo CodRef NroRef CodDept CodProv CodDist CodPos FlgEnv Cmpbnte TpoLic CodVen Hora Libre_c01 Libre_c02 Libre_c03 Libre_c04 Libre_c05 Libre_d01 Libre_d02 Libre_f01 Libre_f02 Glosa CodTer Importe O:\on_in_co\APLIC\nxtl\dcdocu.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "nxtl/dcdocu.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server Client NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH CDocu NO-LOCK INDEXED-REPOSITION ,    ; NroPed CodVen CodCli NomCli FmaPgo ImpTot FchPed INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI Llave01 Llave02 Llave03 Llave04 Llave05 Llave06 Llave07 Llave08 qDataQuery x  T4  �  B      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
 pcViewColList       ��      |        pcRelative  �  ��      �        pcSdoName       ��      �  �     
 pcSdoName       ��      �        plForwards      ��              pcContext       ��      0        plUpdate    `  ��      T        pcFieldList �  ��      x        pcFieldList     ��      �        pcFieldList �  ��      �        piocContext �  ��      �        piocContext   ��              piocContext 8  ��      ,        piocContext \  ��      P        piocContext �  ��      t        piocContext �  ��      �  �     
 piocContext     ��      �        piocContext     ��      �        pcState     ��               pcContext   0  ��      $        piStartRow  T  ��      H        piStartRow  x  ��      l        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow      ��      �  �     
 piStartRow      ��      �  �     
 phRowObjUpd     ��               pcProperties    T  ��      H        piStartRow  x  ��      l        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow      ��      �  �     
 piStartRow  ,  ��               pcRowIdent      ��      D        pcRowIdent  t  ��      h        pcRowIdent  �  ��      �        pcRowIdent      ��      �        pcRowIdent  �  ��      �        pcValueList     ��      �        pcValueList 4  ��              pcPropertiesForServer       ��      L        pcPropertiesForServer   �  ��      |        pcFieldList �  ��      �        pcFieldList �  ��      �        pcFieldList     ��      �        pcFieldList   ��              pcWhere     ��      ,        pcWhere     ��      L        pcState     ��      l       
 phRowObjUpd �  ��      �       
 phRowObj    �  ��      �       
 phRowObj    �  ��      �        phRowObj        ��      �        phRowObj        ��       	        pioCancel       ��      D	        pcRelative      ��      h	       
 phFilterContainer       ��      �	       
 phRowObjUpd �	  ��      �	        pcRowIdent      ��      �	        pcRowIdent      ��       
       
 phAppService        ��      (
        pcMode  T
  ��      H
       
 phSource    x
  ��      l
        phSource        ��      �
       
 phSource    �
  ��      �
        pcText  �
  ��      �
        pcText      ��      �
        pcText     ��             
 phObject    D  ��      8       
 phObject        ��      \        phObject        ��      �        pcField     ��      �        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller      ��              phCaller        ��      0        phCaller    \  ��      T        pcMod   |  ��      t        pcMod       ��      �       
 pcMod   �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    (  ��               pdRow       ��      @        pdRow       ��      `       
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   \	  t	  v	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props y  z  {  |  ~    �  �  �  �  �  �  �  �  �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �  �  �           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable             |     cQueryString    |  �  "   Y   �          �                  initProps   y  z  �  �  �  $  %  '  (  5  6  =  @  B  D  G  R  �  =  I  �  �  �  �  �  �  �  �  �  �  �  �  �              d     lRet              �        piTableIndex    �  �  (   Z   P  l      �                  deleteRecordStatic  0  1  M  N  j  k  �  �  �  �  �  �  �  �  �  �      5  6  R  S  o  p  �  �  �  �  �  �  �  �           :  ;  =  >                 !       �  �     [       x      �                  pushRowObjUpdTable  �  �        �        pcValType                  $       �  \     \       �      D                  pushTableAndValidate    �  �  �  �        �        pcContext   �             $       �        �        pcMessages            �        pcUndoIds     ,     ]       h                        remoteCommit    �  �  �  �  �  X             $       |        p        pcMessages            �        pcUndoIds   �  �     ^       @      �                  serverCommit        �  ,     _                                 getRowObjUpdStatic  #  %  �  p     `               d                  disable_UI  r  s  4  �%       L       T%                         �  �  M   CDocu   \         d         l         t         |         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �                                              0      "   8      "   @      "   H      "   T         d         l         t         |         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �                        "                     $         ,         4         <         D         L         T         \         d         l         t         |         �         �         �         �         �         �         �         �         �         �         �        CodCia  CodDiv  CodSed  CodPed  NroPed  FchPed  FchVen  FchEnt  CodCli  NomCli  DirCli  Sede    LugEnt  LugEnt2 RucCli  DniCli  NroOrd  CodAlm  CodMon  TpoCmb  Usuario UsrAnu  UsrMod  UsrDscto    UsrAprobacion   FchCre  FchAnu  FchMod  FchDscto    FchAprobacion   Observa FlgIgv  ImpBrt  ImpDto  PorDto  ImpExo  ImpVta  ImpIgv  PorIgv  ImpIsc  ImpFle  ImpInt  ImpTot  SdoAct  ImpCto  PesMat  MrgUti  CodOri  NroOri  FlgEst  FlgSit  FchSit  NroCard FmaPgo  CodRef  NroRef  CodDept CodProv CodDist CodPos  FlgEnv  Cmpbnte TpoLic  CodVen  Hora    Libre_c01   Libre_c02   Libre_c03   Libre_c04   Libre_c05   Libre_d01   Libre_d02   Libre_f01   Libre_f02   Glosa   CodTer  Importe          RowObject   �         �         �         �         �         �         �         �         �         �                            NroPed  CodVen  CodCli  NomCli  FmaPgo  ImpTot  FchPed  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     (  4     RowObjUpd   �         �         �         �         �         �                                                  $          0          <          NroPed  CodVen  CodCli  NomCli  FmaPgo  ImpTot  FchPed  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   l           `   
   appSrvUtils �        �      xiRocketIndexLimit  �         �   
   gshAstraAppserver   �         �   
   gshSessionManager   !        �   
   gshRIManager    0!        !  
   gshSecurityManager  X!        D!  
   gshProfileManager   �!  	 	     l!  
   gshRepositoryManager    �!  
 
     �!  
   gshTranslationManager   �!        �!  
   gshWebManager   �!        �!     gscSessionId    "        "     gsdSessionObj   @"        0"  
   gshFinManager   d"        T"  
   gshGenManager   �"        x"  
   gshAgnManager   �"        �"     gsdTempUniqueID �"        �"     gsdUserObj  �"        �"     gsdRenderTypeObj    #        #     gsdSessionScopeObj  8#       0#  
   ghProp  X#       L#  
   ghADMProps  |#       l#  
   ghADMPropsBuf   �#       �#     glADMLoadFromRepos  �#       �#     glADMOk �#       �#  
   ghContainer  $    	   �#     cObjectName $    
   $     iStart  <$       0$     cAppService \$       P$     cASDivision �$       p$     cServerOperatingMode    �$       �$     cContainerType  �$       �$     cQueryString    �$       �$  
   hRowObject  %       %  
   hDataQuery  0%       $%     cColumns             D%     cDataFieldDefs  l%    H  d%  CDocu   �%    X  |%  RowObject         X  �%  RowObjUpd          "   E   �   �   �   �   *  +  ,  -  D  P  Q  R  T  V  W  X  \  ]  `  a  b  c  e  g  i  k  l  m  p  r  s  u  v  w  x  y    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  !	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  M
  N
  P
  Q
  R
  S
  T
  U
  W
  X
  Y
  Z
  [
  \
  ]
  ^
  _
  `
  a
  b
  c
  d
  e
  f
  g
  h
  i
  j
  k
  l
  m
  n
  o
  q
  r
  s
  t
  u
  v
  w
  x
  y
  z
  {
  |
  }
  ~
  
  �
  �
  �
  �
  �
  �
                     	  
                                �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  9  �  �  �  �  �  �  �        *  <  [  ]  r  �      -  =  >  ?  B  C  D  H  K  L  i  }  �  /  0  <  `  �  �  �  �  �  M  S  T  U  V  [  a  h  �  �  �  �      &  @  J  d  e  o  �  �  �  �  �  �  �      Y  O:\on_in_co\APLIC\nxtl\dcdocu.w  �)  ��  C:\Progress\OpenEdge\src\adm2\data.i �)  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    ,*  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i l*  �
 , O:\on_in_co\APLIC\nxtl\dcdocu.i  �*  �:   C:\Progress\OpenEdge\src\adm2\query.i    �*  z + C:\Progress\OpenEdge\src\adm2\delrecst.i  +  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  4+   ) %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   h+  � ! C:\Progress\OpenEdge\src\adm2\dataquery.i    �+  �Z ( %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   �+  �< " C:\Progress\OpenEdge\src\adm2\appserver.i    $,  �� ' %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   \,  I� # C:\Progress\OpenEdge\src\adm2\smart.i    �,  Ds & C:\Progress\OpenEdge\gui\fn  �,  tw % %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �,  Q. $ C:\Progress\OpenEdge\gui\set <-  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i d-  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    �-  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    �-  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i   .  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i T.  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i �.   
 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    �.  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   /  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   X/  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �/  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �/  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    0  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i \0  �j  C:\Progress\OpenEdge\gui\get �0  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �0  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �0  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i @1  Su  C:\Progress\OpenEdge\src\adm2\globals.i  t1  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �1  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �1  �  C:\Progress\OpenEdge\src\adm2\appsprto.i ,2  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   `2  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    �2  ª  %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   �2  �� 	 C:\Progress\OpenEdge\src\adm2\qryprto.i  ,3  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   `3  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i �3  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    �3  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   4  c�    O:\on_in_co\APLIC\nxtl\dcdocu_cl.w          y      �4  �   -     �4  [  �     �4     �  &   �4  �   S     �4     �  .   �4  �   �     �4     �     �4  �   �     5     �  $   5  �   �     $5     �  $   45  �   �     D5     g  $   T5  �   d     d5     B  $   t5  �   @     �5       $   �5  �        �5     �  $   �5  �   �     �5     �  $   �5  �   �     �5     �  $   �5  �   �     6     �  -   6  �   �     $6     �  ,   46  k   F     D6  �  :      T6        +   d6  �        t6       +   �6  �         �6     �  +   �6  �  �      �6     �  +   �6  �  �      �6     �  +   �6  �  �      �6     �  +   7  �  �      7     r  +   $7  �  o      47     U  +   D7  �  R      T7     8  +   d7  �  5      t7       +   �7  �        �7     �  +   �7  �  �      �7     �  +   �7  �  �      �7     �  +   �7  �  �      �7     �  +   8  �  �      8     �  +   $8  �  �      48     m  +   D8  �  j      T8     P  +   d8  �  M      t8     3  +   �8  �        �8     �  $   �8  �  �      �8     �  $   �8  k  �      �8     �  $   �8  j  �      �8     d  $   9  i  c      9     A  $   $9  _  7      49       *   D9  ^        T9     �  *   d9  ]  �      t9     �  *   �9  \  �      �9     �  *   �9  [  �      �9     u  *   �9  Z  t      �9     N  *   �9  Y  M      �9     '  *   :  X  &      :        *   $:  W  �      4:     �  *   D:  V  �      T:     �  *   d:  U  �      t:     �  *   �:  T  �      �:     d  *   �:  S  c      �:     =  *   �:  R  <      �:       *   �:  Q        �:     �  *   ;  P  �      ;     �  *   $;  O  �      4;     �  *   D;  N  �      T;     z  *   d;  @  l      t;     J  $   �;          �;     �  $   �;    �      �;     �  $   �;  �   I      �;     �  )   �;  g   �      �;  a   �  !   <     |  (   <  _   z  !   $<     X  $   4<  ]   V  !   D<     4  $   T<  I      !   d<  �     "   t<     �  '   �<  �   �  "   �<     �  $   �<  �   �  "   �<     u  $   �<  �   s  "   �<     Q  $   �<  g   7  "   �<          =  O      "   =  �   �  #   $=     �  &   4=  �   X  #   D=        %   T=  �   �  #   d=     �  $   t=  �   �  #   �=     �  $   �=  �   �  #   �=     �  $   �=  �   �  #   �=     j  $   �=  �   V  #   �=     4  $   �=  }   (  #   >       $   >     �  #   $>     <  "   4>     �  !   D>     �      T>     B     d>  �   9     t>  O   +     �>          �>     �     �>  �   �     �>  �   �     �>  O   |     �>     k     �>          �>  y   �
     ?  �   �
  
   ?  G   �
     $?     �
     4?     �
     D?  c   %
  
   T?  x   
     d?  M   
     t?     �	     �?     �	     �?  a   �	     �?  �  s	     �?     T	     �?  �  !	     �?  O   	     �?     	     �?     �     @  �   �     @     �     $@          4@  x   �     D@     �     T@     o     d@     k     t@     W     �@     >     �@  Q   .     �@     �     �@     �     �@     �     �@     n     �@  ]   h  
   �@     ^     A       
   A          $A     �  
   4A  Z   �     DA       	   TA     �     dA     �     tA     �     �A  c   r     �A     P     �A          �A     �      �A     �      �A     �      �A     &      �A           B           