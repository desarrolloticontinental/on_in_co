	��V�$P�7  � �                                              7 37F800EFutf-8 MAIN O:\on_in_co\APLIC\vta\dConPedCre.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,CodCia integer 0 0,CodDiv character 1 0,CodDoc character 2 0,NroPed character 3 0,CodCli character 4 0,NomCli character 5 0,FchPed date 6 0,fchven date 7 0,FchEnt date 8 0,FchAprobacion date 9 0,FlgEst character 10 0,FlgSit character 11 0,ImpTot decimal 12 0,CodMon integer 13 0,CodVen character 14 0,FmaPgo character 15 0,usuario character 16 0,RowNum integer 17 0,RowIdent character 18 0,RowMod character 19 0,RowIdentIdx character 20 0,RowUserProp character 21 0,ChangedFields character 22 0       h-              �             � h-  L�              4�              �@     +   � �  W   �� `  X   �   Y   ��   [   �   \   $� <  ]   `�    ^   �� 0  `   ? �� �  iSO8859-1                                                                           �,    �                                      �                   �                �,  �    �   ޜ   T�              ��  �   0-      <-          �                                             PROGRESS                         L           
    
                    �              �                                                                                                     
               INTEGRAL                         PROGRESS                         |        �                                �P               �                              �  l                      �  |  O     CODCIACODDOCNROPEDFCHPEDCODCLINOMCLIDIRCLIRUCCLIHORAFLGESTCODALMCODMONTPOCMBUSUARIOGLOSAIMPBRTIMPEXOPORIGVIMPDTOIMPTOTIMPIGVCODVENIMPISCIMPVTAFLGSITFMAPGOFCHVENORDCMPCODDIVTIPVTAPORDTOTPOPEDUSRDSCTOLUGENTCODTRANSNROREFCMPBNTEOBSERVANCMPBNTEATENCIONFAXCLILUGENT2IMPFLEFLGIGVTPOLICUBIGEOACUBONNROCARDTIPBONIMPORTEPORCENTUSRAPROBACIONFCHAPROBACIONFCHENTCODPOSFLGENVUSRSACFECSACHORSACLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02USRCHQFCHCHQHORCHQSEDEDIVDESCODREFIMPDTO2GLOSAIMPDTO2FLGIMPODUSRIMPODFCHIMPOD                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /         0 
        1          2 
        3         4         5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          P   "       �        t  
    
                  `  (	             �                                                                                                    
  �	         	  
    
                  	  �	             �	                                                                                                    
  P
  )      �	  
    
                  �	  �
             <
                                                                                          )          
  �
  6      x
  
    
                  d
  ,             �
                                                                                          6          
  �  I      $  
    
                    �             �                                                                                          I          
  T  [      �  
    
                  �  �  	           @                                                                                          [          
     p      |  
    
                  h  0  
           �                                                                                          p          
  �  �      (  
    
                    �             �                                                                                          �          
  X  �      �                         �  �             D                                                                                          �              �      �                        l  4             �                                                                                          �            �  �      ,  
    
                    �             �                                                                                          �          
  \  �      �  
    
                  �  �             H                                                                                          �          
    �      �  
    
                  p  8             �                                                                                          �          
  �  �      0                          �             �                                                                                          �            `  �      �                        �  �             L                                                                                          �              �      �                        t  <             �                                                                                          �                      4                           �             �                                                                                                                �       �  X  �   3   �   �  B�      !         �             �          t      �              �       �  X  �+  4   �+  �  J�      L,         �         �    p!          P#      �                 h�                                               l�            \  L l�                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                                                                                                                                                                                                                                           h  p  t  |  x          �             �  �  �  �  �          �             �  �  �    �                               4  $                         8  @  H  X  P          \             p  x  �  �  �          �             �  �  �  �  �                         �  �  �                                  (  8                             @  P  \  l      t  p                 �  �  �  �  �                         �  �  �  �  �                         �  �  �                                (  ,  <  4          @             T  \  d  |  p                         �  �  �  �  �                         �  �  �  �  �                         �  �  �  �                                                                       $   ,   4   <                              @   L   T   `                              d   p   x   �                                                                           CodCia  999 Cia Cia 0   C�digo de Compa�ia  CodDiv  x(5)    Division    Division    00000   Ingrese el Codigo de Division   CodDoc  x(3)    Codigo  Codigo      NroPed  X(9)    No. Pedido  Numero!Pedido       CodCli  x(11)   Codigo  Codigo      C�digo del Cliente  NomCli  x(50)   Nombre  Nombre      Nombre del Cliente  FchPed  99/99/9999  Fecha   Fch.Pedido  today   fchven  99/99/99    Fecha Vencimiento   Fecha!Vcmto.    ?   FchEnt  99/99/9999  Fecha Entrega   TODAY   FchAprobacion   99/99/99    FchAprobacion   ?       Fecha que se aprobo el pedido .     FlgEst  X(1)    Estado  Estado  P   FlgSit  X   Situaci�n   Situaci�n       ImpTot  ->>,>>>,>>9.99  Importe Total   Importe Total   0   CodMon  9   Moneda  Cod!mon 1   C�digo de moneda    CodVen  x(10)   Vendedor    Vendedor        FmaPgo  X(8)    Condicion de ventas Condicion de!venta      usuario x(10)   usuario usuario     RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  &�  ��������� 00000    ����P               \        l        s                �     i     i     i     	 	 	    �  �  �  �  �             *  1  8  ?  F  M  T  \  c  l  s                                                                                                                                       	                  
                                                                                                                                                                                                                                                                                             p'  x'  |'  �'  �'          �'             �'  �'  �'  �'  �'          �'             �'  �'  �'  (  (                         (  (   (  <(  ,(                         @(  H(  P(  `(  X(          d(             x(  �(  �(  �(  �(          �(             �(  �(  �(  �(  �(                         �(  �(  �(  )  )                         )  $)  0)  @)                             H)  X)  d)  t)      |)  x)                 �)  �)  �)  �)  �)                         �)  �)  �)  �)  �)                         �)  �)  *  $*  *                         (*  0*  4*  D*  <*          H*             \*  d*  l*  �*  x*                         �*  �*  �*  �*  �*                         �*  �*  �*  �*  �*                         �*  �*  �*  +                             +  +  +  (+                              ,+  4+  <+  D+                             H+  T+  \+  h+                             l+  x+  �+  �+                              �+  �+  �+  �+                                                                          CodCia  999 Cia Cia 0   C�digo de Compa�ia  CodDiv  x(5)    Division    Division    00000   Ingrese el Codigo de Division   CodDoc  x(3)    Codigo  Codigo      NroPed  X(9)    No. Pedido  Numero!Pedido       CodCli  x(11)   Codigo  Codigo      C�digo del Cliente  NomCli  x(50)   Nombre  Nombre      Nombre del Cliente  FchPed  99/99/9999  Fecha   Fch.Pedido  today   fchven  99/99/99    Fecha Vencimiento   Fecha!Vcmto.    ?   FchEnt  99/99/9999  Fecha Entrega   TODAY   FchAprobacion   99/99/99    FchAprobacion   ?       Fecha que se aprobo el pedido .     FlgEst  X(1)    Estado  Estado  P   FlgSit  X   Situaci�n   Situaci�n       ImpTot  ->>,>>>,>>9.99  Importe Total   Importe Total   0   CodMon  9   Moneda  Cod!mon 1   C�digo de moneda    CodVen  x(10)   Vendedor    Vendedor        FmaPgo  X(8)    Condicion de ventas Condicion de!venta      usuario x(10)   usuario usuario     RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  &�  ��������� 00000    ����P               \        l        s                �     i     i     i     	 	 	    �  �  �  �  �             *  1  8  ?  F  M  T  \  c  l  s    �    ��                            ����                            c    ��                    b�    �   ��                    �Q    undefined                                                               �       ��  �   l   ��  ��                    �����               �c[                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       �     9          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   �Q                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  P  S  L              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  U  [  �              8�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                  ]  ^  p              DA                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  `  c  p              �C                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  e  g  �              �/                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  i  l  �	              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  n  o  H              p�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  q  r  T              (]                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  t  v  T              �]                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                  x  y  |              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  {  |  |              t�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  ~    |               �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                  �  �  |              ��`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              T�`                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �              ��`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              `�`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              �`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              ``                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              w`                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `              ܮ_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d              ̿`                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �              $�_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              ��_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              X�b                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              �Jb                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              �`                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              pv_                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                  �  �  @$              , `                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                  �  �  l%              4�b                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                  �  �  d'              Xf_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                  �  �  h(              �J`                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                  �  �  �)              <�`                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                  �  �  @+              h�^                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                  �  �  �,              ��b                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                      �-              ��b                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                  	  
  �/              ��`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                      �0              d�`                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                      �1              �Q_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                      �2              4R_                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3     n       CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	 u       CHARACTER,  canNavigate �3      �3      (4           LOGICAL,    closeQuery  4      44      `4   
 �       LOGICAL,    columnProps @4      l4      �4    �       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	 �       CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7    �       LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7    �       LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7    �       CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8           CHARACTER,  hasForeignKeyChanged    88      d8      �8          LOGICAL,    openDataQuery   |8      �8      �8    ,      LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	 :      LOGICAL,    prepareQuery    9      49      d9    D      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    Q      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 ^      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	 h      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	 r      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;    |      CHARACTER,  assignDBRow                             <  �;      ��                  �  �  <              pwa                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                       L=              D�`                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                      �>              �l_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                  
    �?              �m_                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                       A              �7b                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                      PB              �a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                      PC              ��a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                      PD              d_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                      PE              l_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                      PF              ��_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                  !  #  \G              Ě_                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  %  &  �H              �4_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  (  *  �I              �5_                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  ,  -  �J              h"`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  /  0  �K              �"`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  2  5  �L              �#`                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M    �      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP          CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P          CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q     %      CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !  2      CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "  C      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  R      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  a      CHARACTER,  getForeignValues    @R      lR      �R  %  p      CHARACTER,  getQueryPosition    �R      �R      �R  &  �      CHARACTER,  getQuerySort    �R      �R      S  '  �      CHARACTER,  getQueryString  �R      (S      XS  (  �      CHARACTER,  getQueryWhere   8S      dS      �S  )  �      CHARACTER,  getTargetProcedure  tS      �S      �S  *  �      HANDLE, indexInformation    �S      �S      T  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .         CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /        CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0        CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1  .      CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2  >      LOGICAL,    removeQuerySelection    �W      �W      (X  3  O      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  d      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
 r      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6  }      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7  �      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8  �      LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              �ub                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\              hvb                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                  �  �  �]              wb                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                  �  �  �^              �a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                  �  �  `              D	a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                  �  �  a              |�`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                  �  �  b              ��_                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                  �  �  Dc              `�`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                  �  �  Hd              x�`                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <  �      CHARACTER,  getASBound  �d      e      0e  = 
 �      LOGICAL,    getAsDivision   e      <e      le  >  �      CHARACTER,  getASHandle Le      xe      �e  ?         HANDLE, getASHasStarted �e      �e      �e  @        LOGICAL,    getASInfo   �e      �e      f  A 	       CHARACTER,  getASInitializeOnRun    �e       f      Xf  B  &      LOGICAL,    getASUsePrompt  8f      df      �f  C  ;      LOGICAL,    getServerFileName   tf      �f      �f  D  J      CHARACTER,  getServerOperatingMode  �f      �f      g  E  \      CHARACTER,  runServerProcedure  �f      $g      Xg  F  s      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G  �      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              �^a                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl              l�a                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              �`                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o              ,�_                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p              ��_                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              0�_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              (�a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              ԅa                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t              ��a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u              p
b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                  �  �  �v              b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                  �  �  �w              x�`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                  �  �  �x              `�`                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                  �  �  |z              x�a                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                  �  �  �{              �/b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                  �  �  �|              �[`                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                  �  �  x~              @a_                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                  �  �  �              p�_                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                  �  �  ��              �_                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                       L�              �b                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                      t�              ��b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
 S      LOGICAL,    assignLinkProperty  ؃      �      8�  P  ^      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  Q  q      CHARACTER,  getChildDataKey ��      ̄      ��  R        CHARACTER,  getContainerHandle  ܄      �      <�  S  �      HANDLE, getContainerHidden  �      D�      x�  T  �      LOGICAL,    getContainerSource  X�      ��      ��  U  �      HANDLE, getContainerSourceEvents    ��      ��      ��  V  �      CHARACTER,  getContainerType    ܅      �      <�  W  �      CHARACTER,  getDataLinksEnabled �      H�      |�  X  �      LOGICAL,    getDataSource   \�      ��      ��  Y        HANDLE, getDataSourceEvents ��      ��      �  Z        CHARACTER,  getDataSourceNames  Ԇ       �      4�  [  (      CHARACTER,  getDataTarget   �      @�      p�  \  ;      CHARACTER,  getDataTargetEvents P�      |�      ��  ]  I      CHARACTER,  getDBAware  ��      ��      �  ^ 
 ]      LOGICAL,    getDesignDataObject ȇ      �      (�  _  h      CHARACTER,  getDynamicObject    �      4�      h�  `  |      LOGICAL,    getInstanceProperties   H�      t�      ��  a  �      CHARACTER,  getLogicalObjectName    ��      ��      ��  b  �      CHARACTER,  getLogicalVersion   Ј      ��      0�  c  �      CHARACTER,  getObjectHidden �      <�      l�  d  �      LOGICAL,    getObjectInitialized    L�      x�      ��  e  �      LOGICAL,    getObjectName   ��      ��      �  f  �      CHARACTER,  getObjectPage   ̉      ��      (�  g  �      INTEGER,    getObjectParent �      4�      d�  h        HANDLE, getObjectVersion    D�      l�      ��  i        CHARACTER,  getObjectVersionNumber  ��      ��      �  j  ,      CHARACTER,  getParentDataKey    Ċ      ��      $�  k  C      CHARACTER,  getPassThroughLinks �      0�      d�  l  T      CHARACTER,  getPhysicalObjectName   D�      p�      ��  m  h      CHARACTER,  getPhysicalVersion  ��      ��      �  n  ~      CHARACTER,  getPropertyDialog   ȋ      �      (�  o  �      CHARACTER,  getQueryObject  �      4�      d�  p  �      LOGICAL,    getRunAttribute D�      p�      ��  q  �      CHARACTER,  getSupportedLinks   ��      ��      ��  r  �      CHARACTER,  getTranslatableProperties   ��      �      (�  s  �      CHARACTER,  getUIBMode  �      4�      `�  t 
 �      CHARACTER,  getUserProperty @�      l�      ��  u  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    |�      č      ��  v  		      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ܍      $�      P�  w  	      CHARACTER,INPUT pcLink CHARACTER    linkProperty    0�      t�      ��  x  *	      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      ��      �  y  7	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      x�      ��  z  C	      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ̏      ��  {  Q	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ܏      $�      T�  |  ^	      CHARACTER,  setChildDataKey 4�      `�      ��  }  m	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  p�      ��      �  ~  }	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ̐      �      @�    �	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents     �      `�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled |�      ��      ��  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ԑ      �      L�  �  �	      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ,�      l�      ��  �  �	      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      Ȓ      ��  �  �	      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ܒ      $�      T�  �  
      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 4�      x�      ��  �  
      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Г      ��  � 
 '
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ܓ      �      P�  �  2
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    0�      x�      ��  �  F
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      Ȕ       �  �  W
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      $�      \�  �  m
      LOGICAL,INPUT c CHARACTER   setLogicalVersion   <�      x�      ��  �  �
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Е       �  �  �
      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��       �      P�  �  �
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    0�      p�      ��  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ̖       �  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      (�      \�  �  �
      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   <�      |�      ��  �  �
      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ԗ      �  �  �
      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      ,�      \�  �        LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   <�      ��      ��  �  !      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ܘ      �  �  3      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      <�      h�  � 
 M      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  X      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �  h      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	 t      CHARACTER,INPUT pcName CHARACTER    l�      ��  0�      �       4   �����                 @�                      ��                    I                  |�`                         Ě          \�  ؛      �       4   �����                 �                      ��                    H                   �`                         l�  �    5  �  ��      �       4   �����                 ��                      ��                  A  C                  ��`                       A  �         B                                  ,     
                    � ߱        �  $  E  ��  ���                           $  G  @�  ���                       x                         � ߱        x�    M  ��  �      �      4   �����                �                      ��                  N  	                  8�`                       N  ��  H�  o   Q      ,                                 ��  $   R  t�  ���                       �  @         �              � ߱        ��  �   S        Ȟ  �   T  �      ܞ  �   V        �  �   X  x      �  �   Z  �      �  �   \  `      ,�  �   ]  �      @�  �   ^        T�  �   a  �      h�  �   c         |�  �   d  |      ��  �   f  �      ��  �   g  t      ��  �   h  �      ̟  �   i  ,      ��  �   j  �      ��  �   p  �      �  �   r  P	      �  �   x  �	      0�  �   z   
      D�  �   |  t
      X�  �   }  �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  9	  g	  ��              <�b                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        h�  $ M	  آ  ���                           O   e	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ  |                     8�    �	  ��  �      t      4   ����t                 �                      ��                  �	  
                  �a                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    >
  T�  Ц      x      4   ����x                �                      ��                  ?
  �
                  oa                       ?
  d�  ��  �   A
  �      �  �   B
  T      �  �   C
  �      0�  �   D
  D      D�  �   E
  �      X�  �   F
  �      l�  �   H
  p      ��  �   I
  �      ��  �   J
  X      ��  �   K
  �      ��  �   L
  �      Ч  �   M
  D       �  �   N
  �       ��  �   O
  �       �  �   P
  x!       �  �   Q
  �!      4�  �   R
  h"      H�  �   S
  �"      \�  �   T
  `#      p�  �   U
  �#      ��  �   V
  X$      ��  �   W
  �$      ��  �   X
  �$      ��  �   Y
  L%      Ԩ  �   Z
  �%      �  �   [
  <&      ��  �   \
  �&      �  �   ]
  4'      $�  �   ^
  �'      8�  �   _
  ,(      L�  �   `
  h(      `�  �   b
  �(      t�  �   c
  X)      ��  �   d
  �)      ��  �   e
  *      ��  �   f
  �*      ĩ  �   g
  �*      ة  �   h
  l+      �  �   i
  �+       �  �   j
  \,      �  �   k
  �,      (�  �   l
  L-      <�  �   m
  �-      P�  �   n
  <.      d�  �   o
  �.      x�  �   p
  4/      ��  �   q
  �/          �   r
  $0      d�    �
  ��  8�      T0      4   ����T0                H�                      ��                  �
  {                  ra                       �
  ̪  \�  �   �
  �0      p�  �   �
  (1      ��  �   �
  �1      ��  �   �
  2      ��  �   �
  �2      ��  �   �
  3      ԫ  �   �
  |3      �  �   �
  �3      ��  �   �
  t4      �  �   �
  �4      $�  �   �
  l5      8�  �   �
  �5      L�  �   �
  d6      `�  �   �
  �6      t�  �   �
  L7      ��  �   �
  �7      ��  �      <8      ��  �     �8      Ĭ  �     ,9      ج  �     �9      �  �     :       �  �     X:      �  �     �:      (�  �     H;      <�  �     �;      P�  �   	  8<          �   
  �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  *                  �a                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �    �  ��  ��      K      4   ����K      /   �  а     �                          3   ����K             �                      3   ����<K  Զ    �  ,�  ��  �  XK      4   ����XK  
              ��                      ��             
     �  L                  \a                       �  <�  ̱  �   �  �K      $�  $  �  ��  ���                       �K     
                    � ߱        8�  �   �  L      ��  $   �  d�  ���                       ,L  @         L              � ߱        L�  $  �  ��  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱        ܳ  V   �  �  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        l�  $    x�  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   -  �  ���                                      ̵                      ��                  N  �                  ��_                       N  ��  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   c  �  ���                        adm-clone-props �  ��              �     W     `                          \  r                     start-super-proc    �  d�  �           �     X                                  �                     l�      �   �      �X      4   �����X      /     ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $    ��  ���                       @Y                         � ߱        ��    .  �  \�  ��  \Y      4   ����\Y                и                      ��                  /  3                  �%_                       /  �  pY                     �Y                     �Y                         � ߱            $  0  l�  ���                             4  �  T�      �Y      4   �����Y  �Y                         � ߱            $  5  (�  ���                       |�    <  ��  ��  �  �Y      4   �����Y      $  =  ع  ���                       Z                         � ߱            �   Z  Z      XZ     
                �Z                     $\  @        
 �[              � ߱        ��  V   n  �  ���                        ��  �   �  0\      ��       غ  �      p\      4   ����p\      /   !  �     $�                          3   �����\            D�                      3   �����\  �\     
                <]                     �^  @        
 L^              � ߱        H�  V   -  T�  ���                        �^     
                T_                     �`  @        
 d`              � ߱        t�  V   Q  �  ���                        ��    �  ��  �      �`      4   �����`                �                      ��                  �  �                  �kb                       �  ��  ��  /   �  H�     X�                          3   �����`            x�                      3   �����`      /   �  ��     Ľ                          3   ����a            �                      3   ����$a   �  /  >   �         Xa                      3   ����@a  initProps   x�  0�                   Y     �             �          �  n  	                                   t�          �  �      ��                �  �  4�               ~E                    O   ����    e�          O   ����    R�          O   ����    ��      x                      L�          ��  p   �  ,|  ��      �  ��  �     8|                �                      ��                  �  �                  0VE                       �  ��  4�  :  �                 $  �  `�  ���                       L|                         � ߱        �  �     d|                                        ��                  �  �                  D�G                       �  ��  ��  ��     x|                                        ��                  �                    �G                       �  (�  0�   �     �|                                        ��                                      �G                         ��  ��  ��     �|                                        ��                     <                   'E                          @�  H�  8�     �|                                        ��                  =  Y                  �'E                       =  ��  ��  ��     �|                                        ��                  Z  v                  �(E                       Z  X�  `�  P�     �|                                        ��                  w  �                  �)E                       w  ��  ��  ��     �|  	                                      ��             	     �  �                  |�F                       �  p�  x�  h�     }  
                                      ��             
     �  �                  �F                       �  ��  �  ��     }                                        ��                  �  �                  ��F                       �  ��  ��  ��     ,}                                        ��                  �                    � G                       �  �  �  �     @}                                        ��                    $                  xG                         ��  ��  ��     T}                                        ��                  %  A                  MF                       %  ,�  4�  $�     h}                                        ��                  B  ^                  �MF                       B  ��  ��  ��     |}                                        ��                  _  {                  �NF                       _  D�  L�  <�     �}                                        ��                  |  �                  �OF                       |  ��      ��     �}                                        ��                  �  �                  \PF                       �  \�      O   �  ��  ��  �}               \�          D�  P�   , $�                                                       �     ��                            ����                            <�  d�  X�  ��      ��     Z     d�                      � `�  �                     ��    �  �  ��      �}      4   �����}                ��                      ��                  �  �                  hG                       �  ,�  �  /   �  ��     ��                          3   �����}            �                      3   �����}  ��  /   �  @�     P�                          3   ����~            p�                      3   ����,~  ��  /   �  ��     ��                          3   ����H~            ��                      3   ����h~      /   �  �     (�                          3   �����~            H�                      3   �����~  �~     
                D                     ��  @        
 T�              � ߱        ��  V   >  X�  ���                        ��  $  X  �  ���                       ��                         � ߱        Ā     
                @�                     ��  @        
 P�              � ߱        ��  V   b  @�  ���                        ��  $  |  ��  ���                       ��     
                    � ߱        ��     
                ,�                     |�  @        
 <�              � ߱        ��  V   �  (�  ���                        t�  $  �  ��  ���                       ��     
                    � ߱        ��     
                �                     h�  @        
 (�              � ߱        ��  V   �  �  ���                        \�  $  �  ��  ���                       ��                         � ߱        ��     
                $�                     t�  @        
 4�              � ߱        ��  V   �  ��  ���                        ��  �   �  ��      X�  $  �  ��  ���                       ��     
                    � ߱        ��     
                <�                     ��  @        
 L�              � ߱        ��  V   �  ��  ���                        ��  $    ��  ���                       ��     
                    � ߱        ��  �   (  ��      H�  $  2  �  ���                       �     
                    � ߱        \�  �   L   �      ��  $  n  ��  ���                       @�                         � ߱              y  ��  ��      \�      4   ����\�      /   z  �     �                          3   ����|�  L�     
   <�                      3   ������  |�        l�                      3   ������  ��        ��                      3   ������            ��                      3   ����ԋ  pushRowObjUpdTable  ��  ��  �                   [      �                               $                     pushTableAndValidate    ��  L�  �           |     \     �                          �  A                     remoteCommit    d�  ��  �           p     ]     �                          �  �                     serverCommit    ��  ,�  �           l     ^     �                          �  �                                     L�          �  �      ��                  �  �  4�              �qG                    O   ����    e�          O   ����    R�          O   ����    ��          O   �  ��  ��  �    ��                            ����                            <�  P�      ��              _      d�                      
�     �                     disable_UI  ��   �                      `      �                               �  
                    �  �    ����  �       ��          ��  8   ����   ��  8   ����   ��  8   ����   ��  8   ����       8   ����       8   ����        �  �      viewObject  ,   ��   �  ,�      toggleData  ,INPUT plEnabled LOGICAL    �  X�  p�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  H�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  8�  D�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE (�  ��  ��      removeAllLinks  ,   ��  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  $�  8�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    �  ��  ��      hideObject  ,   ��  ��  ��      exitObject  ,   ��  ��  �      editInstanceProperties  ,   ��  �  ,�      displayLinks    ,   �  @�  P�      createControls  ,   0�  d�  t�      changeCursor    ,INPUT pcCursor CHARACTER   T�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  @�  L�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER 0�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��   �  �      unbindServer    ,INPUT pcMode CHARACTER ��  8�  H�      runServerObject ,INPUT phAppService HANDLE  (�  t�  ��      disconnectObject    ,   d�  ��  ��      destroyObject   ,   ��  ��  ��      bindServer  ,   ��  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  4�  @�      startFilter ,   $�  T�  d�      releaseDBRow    ,   D�  x�  ��      refetchDBRow    ,INPUT phRowObjUpd HANDLE   h�  ��  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE ��  ��  �      fetchDBRowForUpdate ,   ��  $�  4�      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL �  d�  t�      compareDBRow    ,   T�  ��  ��      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   x�  �  �      assignDBRow ,INPUT phRowObjUpd HANDLE    �  H�  T�      updateState ,INPUT pcState CHARACTER    8�  ��  ��      updateQueryPosition ,   p�  ��  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    ��   �  �      undoTransaction ,   ��  $�  4�      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  �  ��  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   ��  ,�  @�      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   �  ��  ��      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  |�  �  $�      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  �  h�  |�      startServerObject   ,   X�  ��  ��      setPropertyList ,INPUT pcProperties CHARACTER   ��  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��   �  0�      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    �  ��   �      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  ,�  <�      rowObjectState  ,INPUT pcState CHARACTER    �  h�  x�      retrieveFilter  ,   X�  ��  ��      restartServerObject ,   |�  ��  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   ��  ��  ��      refreshRow  ,   ��  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  P�  `�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  @�  ��  ��      initializeServerObject  ,   ��  ��  ��      initializeObject    ,   ��  ��  ��      home    ,   ��   �  �      genContextList  ,OUTPUT pcContext CHARACTER ��  <�  H�      fetchPrev   ,   ,�  \�  h�      fetchNext   ,   L�  |�  ��      fetchLast   ,   l�  ��  ��      fetchFirst  ,   ��  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   ��  ��  �      endClientDataRequest    ,   ��   �  4�      destroyServerObject ,   �  H�  X�      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    8�  ��  ��      dataAvailable   ,INPUT pcRelative CHARACTER ��  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  4�  H�      commitTransaction   ,   $�  \�  l�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    L�  �  �      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 %     adecomm/as-utils.w 
"   
   �    }        �
"     
    �     }        �� ,   A   %               � 
" 	   
 �%              h �P  \         (          
�                          
�            � ~   M
" 	   
 `
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 y�               1� �  
 y� �   �%               o%   o           � �    y
"   
 y�           �    1� �   y� �   �%               o%   o           � �   y
"   
 y�           �    1� �  
 y� �   �%               o%   o           � �   y
"   
 y�           l    1� �   y� �   �%               o%   o           � �    y
"   
 y�           �    1� �   y� �   �%               o%   o           � �   y
"   
 y�           T    1�    y�    �%               o%   o           %               
"   
 ��          �    1�    �� %     
"   
 y�               1� ,   y� �   �%               o%   o           � ?  y
"   
 y�           �    1� A   y� �   �%               o%   o           � P  S y
"   
 y�           �    1� �   y�    �%               o%   o           %               
"   
 y�           p    1� �   y�    �%               o%   o           %               
"   
 y�           �    1� �   y�    �%               o%   o           %              
"   
 ��          h    1� �   ��      
"   
 y�           �    1� �  
 y�    �%               o%   o           %               
"   
 y�                1� �   y� �   �%               o%   o           � �    y
"   
 ��          �    1� �   �� %     
"   
 y�           �    1�    y� �   �%               o%   o           �   t y
"   
 ��          D	    1� �  
 �� %     
"   
 y�           �	    1� �   y� �   �%               o%   o           � �  � y
"   
 y�           �	    1� 9   y� �   �%               o%   o           � �    y
"   
 y�           h
    1� P  
 y� [   �%               o%   o           %               
"   
 ^�           �
    1� _   ^�    �%               o%   o           %              
"   
 `�           `    1� g   `� �   �%               o%   o           � �    ^
"   
 `�           �    1� x   `� �   �%               o%   o           o%   o           
"   
 `�           P    1� �  
 `� �   �%               o%   o           � �    `
"   
 `�           �    1� �   `� �  	 �%               o%   o           � �  / `
"   
 ��          8    1� �   �� �  	   
"   
 `�           t    1� �   `� �  	 �o%   o           o%   o           � �    `
"   
 ��          �    1�    �� �  	   
"   
 b�           $    1�    b� �  	 �o%   o           o%   o           � �    b
"   
 ��          �    1� "   ��      
"   
 ��          �    1� 0   �� �  	   
"   
 ��              1� =   �� �  	   
"   
 ��          L    1� J   �� �  	   
"   
 `�           �    1� X   `�    �o%   o           o%   o           %              
"   
 ��              1� i   �� �  	   
"   
 ��          @    1� w  
 �� �     
"   
 ��          |    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          0    1� �   �� �  	   
"   
 ��          l    1� �  	 �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 `�                1�    `� �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 _
"   
   
"   
 M(�  L ( l       �        �    ��    � P   �        �    �@    
� @  , 
�            ��      p�               �L
�    %              � 8          � $         �            
�    � :     
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 b�           �    1� =  
 b� �   �%               o%   o           � �    b
"   
 b�           <    1� H  
 b� �   �%               o%   o           o%   o           
"   
 a�           �    1� S   a� %   �%               o%   o           o%   o           
"   
 `�           4    1� \   `�    �%               o%   o           %               
"   
 ^�           �    1� k   ^�    �%               o%   o           %               
"   
 �           ,    1� x   � �   �%               o%   o           � �    ^
"   
 `�           �    1�    `�    �%               o%   o           %              
"   
 `�               1� �   `�    �%               o%   o           o%   o           
"   
 `�           �    1� �   `� �   �%               o%   o           o%   o           
"   
 a�               1� �  	 a� �   �%               o%   o           � �    `
"   
 a�           �    1� �   a� �   �%               o%   o           o%   o           
"   
 _�               1� �   _� �   �%               o%   o           o%   o           
"   
 ^�           �    1� �   ^�    �%               o%   o           %               
"   
 ^�           �    1� �   ^�    �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 _�           �    1� �  
 _�    �%               o%   o           %              
"   
 _�           H    1� �   _� �   �%               o%   o           o%   o           
"   
 b�           �    1�    b� �   �%               o%   o           � �    `
"   
 b�           8    1�    b� �   �%               o%   o           o%   o           
"   
 ��          �    1� %   �� %     
"   
 `�           �    1� 2   `� �   �%               o%   o           � E  ! _
"   
 `�           d    1� g   `� �   �%               o%   o           � �    `
"   
 `�           �    1� t   `� �   �%               o%   o           � �   `
"   
 ��          L    1� �   �� �     
"   
 ��          �    1� �   �� %     
"   
 ^�           �    1� �   ^� �   �%               o%   o           � �    a
"   
 ��          8     1� �  
 �� %     
"   
 �           t     1� �   �    �%               o%   o           o%   o           
"   
 `�           �     1� �   `�    �%               o%   o           %               
"   
 b�           l!    1� �   b�    �%               o%   o           %               
"   
 `�           �!    1�     `� �   �%               o%   o           � �    b
"   
 `�           \"    1�    `� �   �%               o%   o           o%   o           
"   
 b�           �"    1�    b�    �%               o%   o           %              
"   
 `�           T#    1� -   `�    �%               o%   o           %               
"   
 �           �#    1� :   �    �%               o%   o           %               
"   
 ��          L$    1� J   �� %     
"   
 ��          �$    1� W   �� �     
"   
 `�           �$    1� d   `� [   �%               o%   o           o%   o           
"   
 `�           @%    1� p   `� �   �%               o%   o           � �    `
"   
 `�           �%    1� ~   `� �   �%               o%   o           o%   o           
"   
 _�           0&    1� �   _�    �o%   o           o%   o           o%   o           
"   
 _�           �&    1� �   _� �  	 �%               o%   o           o%   o           
"   
 b�           ('    1� �   b� �   �%               o%   o           o%   o           
"   
 a�           �'    1� �  
 a� [   �%               o%   o           o%   o           
"   
 ��           (    1� �   �� �     
"   
 `�           \(    1� �   `� �   �%               o%   o           � �  4 b
"   
 `�           �(    1� '  
 `�    �%               o%   o           %              
"   
 ��          L)    1� 2   �� %     
"   
 _�           �)    1� C   _� �   �%               o%   o           � �    
"   
 `�           �)    1� Q   `�    �%               o%   o           %              
"   
 `�           x*    1� `   `� �   �%               o%   o           � �    `
"   
 ^�           �*    1� m   ^� �   �%               o%   o           � �    `
"   
 b�           `+    1� {   b� �   �%               o%   o           � �    ^
"   
 _�           �+    1� �   _�    �%               o%   o           %               
"   
 _�           P,    1� �  	 _� %   �%               o%   o           o%   o           
"   
 �           �,    1� �   � �   �%               o%   o           � �  	 `
"   
 _�           @-    1� �   _� [   �%               o%   o           %       �       
"   
 `�           �-    1� �   `� �   �%               o%   o           � �    _
"   
 `�           0.    1� �   `�    �o%   o           o%   o           %              
"   
 ^�           �.    1� �   ^�    �%               o%   o           %               
"   
 ^�           (/    1� �   ^� �   �%               o%   o           o%   o           
"   
 _�           �/    1�    _� �  	 �%               o%   o           � �    b
"   
 ��          0    1�    �� �  	   P �L 
�H T   %              �     }        �GG %              
"   
 �           �0    1� $  
 � �   �%               o%   o           � �    
"   
 a�           1    1� /   a�    �%               o%   o           %               
"   
 `�           �1    1� <  	 `� �   �%               o%   o           � �    a
"   
 `�           2    1� F   `� �   �%               o%   o           � �    `
"   
 ^�           �2    1� T   ^�    �%               o%   o           %               
"   
 b�           �2    1� d   b� �   �%               o%   o           � �    ^
"   
 b�           p3    1� w   b� �   �%               o%   o           o%   o           
"   
 b�           �3    1�    b� �   �%               o%   o           o%   o           
"   
 `�           h4    1� �   `�    �%               o%   o           o%   o           
"   
 �           �4    1� �   �    �%               o%   o           o%   o           
"   
 a�           `5    1� �   a�    �%               o%   o           o%   o           
"   
 `�           �5    1� �   `� �   �%               o%   o           o%   o           
"   
 _�           X6    1� �  	 _� �  	 �%               o%   o           � �    `
"   
 `�           �6    1� �  
 `� �  	 �%               o%   o           � �    _
"   
 _�           @7    1� �   _� �   �%               o%   o           � �    `
"   
 _�           �7    1� �   _� �   �%               o%   o           o%   o           
"   
 ^�           08    1� �   ^� �   �%               o%   o           o%   o           
"   
 b�           �8    1� 	   b� �   �%               o%   o           � �    b
"   
 `�            9    1�    `� �   �%               o%   o           � �    b
"   
 `�           �9    1� -   `� �  	 �%               o%   o           o%   o           
"   
 ��          :    1� ?   �� %     
"   
 `�           L:    1� K   `� �   �%               o%   o           � �    _
"   
 `�           �:    1� Y   `� �   �%               o%   o           o%   o           
"   
 �           <;    1� l   �    �%               o%   o           o%   o           
"   
 b�           �;    1� ~  
 b� �   �%               o%   o           � �    ^
"   
 a�           ,<    1� �   a� �   �%               o%   o           � �    b
"   
 `�           �<    1� �   `�    �%               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 `�           p=    1� �  	 `� %   �%               o%   o           o%   o           
"   
 `�           �=    1� �   `� %   �%               o%   o           o%   o           
"   
 _�           h>    1� �   _� %   �%               o%   o           o%   o           
"   
 �           �>    1� �   �    �%               o%   o           %              
"   
 ^�           `?    1� �   ^� �   �%               o%   o           �   M 
"   
 b�           �?    1� U   b�    �%               o%   o           %              
"   
 a�           P@    1� f   a�    �%               o%   o           %               
"   
 `�           �@    1� z   `�    �%               o%   o           %               
"   
 _�           HA    1� �   _� �  	 �%               o%   o           � �   `
"   
 _�           �A    1� �   _�    �%               o%   o           %               
"   
 _�           8B    1� �   _� �  	 �%               o%   o           o%   o           
"   
 b�           �B    1� �   b�    �o%   o           o%   o           %              
"   
 �           0C    1� �   � �  	 �o%   o           o%   o           � �    
"   
 ^�           �C    1� �   ^� %   �o%   o           o%   o           o%   o           
"   
 ^�            D    1� �   ^� %   �o%   o           o%   o           o%   o           
"   
 ^�           �D    1�    ^� �  	 �o%   o           o%   o           o%   o           
"   
 ^�           E    1�    ^� %   �o%   o           o%   o           o%   o           
"   
 ^�           �E    1� ,   ^� �  	 �o%   o           o%   o           � :   ^
"   
 b�           F    1� <   b� �  	 �o%   o           o%   o           � K   b
"   
 `�           |F    1� W   `�    �%               o%   o           %               
"   
 `�           �F    1� k   `�    �%               o%   o           %               
"   
 ��          tG    1�    �� �  	   
"   
 `�           �G    1� �   `�    �%               o%   o           %               
"   
 `�           ,H    1� �   `� �   �%               o%   o           o%   o           
"   
 _�           �H    1� �   _� �   �%               o%   o           o%   o           
"   
 _�           $I    1� �   _�    �%               o%   o           o%   o           
"   
 a�           �I    1� �   a� �   �%               o%   o           � �    b
"   
 b�           J    1� �   b� �   �%               o%   o           %               
"   
 `�           �J    1� �  	 `�    �%               o%   o           %                "    �%     start-super-proc ��%     adm2/smart.p �MP �L 
�H T   %              �     }        �GG %              
"   
   �       �K    6�      
"   
   
�        �K    8
"   
   �        �K    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 M
"   
 �
"   
 M
"   
   (�  L ( l       �        �M    ��    � P   �        �M    �@    
� @  , 
�       �M    ��    Mp�               �L
�    %              � 8       N    � $         �            
�    � :   M
"   
 �p� @  , 
�       O    �� ,   �p�               �L"  	  , �   � 4   b� 6   ��     }        �A      |    "  	    � 4   b%              (<   \ (    |    �     }        �A� 8   �A"  
  b    "  	  M"  
  b  < "  	  M"  
  b(    |    �     }        �A� 8   �A"  
  b
�H T   %              �     }        �GG %              
"   
 M
"   
 �
"   
 M
"   
   (�  L ( l       �        �P    ��    � P   �        �P    �@    
� @  , 
�       �P    ��    Mp�               �L
�    %              � 8      Q    � $         �            
�    � :   M
"   
 �p� @  , 
�       R    �� �  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 b
"   
   
"   
   (�  L ( l       �        �R    ��    � P   �        �R    �@    
� @  , 
�       �R    ��      p�               �L
�    %              � 8      �R    � $         �            
�    � :     
"   
 �p� @  , 
�       �S    �� �  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�       XT    �� �     p�               �L%               
"   
  p� @  , 
�       �T    ��     p�               �L%               
"   
  p� @  , 
�       U    �� �    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 _ (   � 
"   
 M    �        �U    ��    �
"   
   � 8      DV    � $         �            
�    � :   M
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6�      
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    � a   b
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 M    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 _"      �       }        �
"   
 �%              %                "    �%     start-super-proc ��%     adm2/appserver.p �b�    � �     
�    �     }        �%               %      Server  - �     }        �    "    `� �    �%                   "    `� �    �%      NONE    p�,  8         $     "    `        � �   M
�    
�H T   %              �     }        �GG %              
"   
 M
"   
 �
"   
 M
"   
   (�  L ( l       �        �Z    ��    � P   �        �Z    �@    
� @  , 
�       �Z    ��    Mp�               �L
�    %              � 8      �Z    � $         �            
�    � :   M
"   
 �p� @  , 
�       �[    �� �   �p�               �L"    , p�,  8         $     "    `        � 
   M
�     "    �%     start-super-proc ��%     adm2/dataquery.p �
�H T   %              �     }        �GG %              
"   
 M
"   
 �
"   
 M
"   
 M(�  L ( l       �        ]    ��    � P   �        ]    �@    
� @  , 
�       $]    ��    Mp�               �L
�    %              � 8      0]    � $         �     M     
�    � :   M
"   
 �p� @  , 
�       @^    �� �   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 M
"   
 �
"   
 M
"   
 M(�  L ( l       �        $_    ��    � P   �        0_    �@    
� @  , 
�       <_    ��    Mp�               �L
�    %              � 8      H_    � $         �     M     
�    � :   M
"   
 �p� @  , 
�       X`    ��    �p�               �L%               "    �%     start-super-proc ��%     adm2/query.p �M%     start-super-proc ��%     adm2/queryext.p % 	    initProps M
�    %8 , (   FOR EACH FacCPedi NO-LOCK INDEXED-REPOSITION �M�   � x     � z     �       
�     	         �G
"   
 _�        �a    �G
"   
   
"   
    x    (0 4      �        �a    �G%                   �        b    �GG %              � ^    M� _         %              %                   "      %              
"   
       "      �        �b    �
"   
   �        ,c    �
"   
   
�       Lc    �"       \      H   "    M((       "      %              � �      � x   M     
"   
   
"   
 � \      H   "      ((       "      %              � �     � x   ^�        �c    �%                   %              %                   "  (    %                  "  (        
"   
 M
"   
 ^0 T       m � "  (  �        �d    �A @   "      $         � "  (  ^� 8   ��        e    �� "  (    
"   
 � \ H     H   "      ((       "    M%              � �    �� x     (        "  !  M� �    ^�        �e    �"  !  �
�H T   %              �     }        �GG %              
"   
 G
"   
 G
"   
   
"   
   (�  L ( l       �        �f    ��    � P   �        �f    �@    
� @  , 
�       �f    ��      p�               �L
�    %              � 8      �f    � $         �            
�    � :     
"   
 �p� @  , 
�       �g    �� �   �p�               �L%               
"   
   p� @  , 
�       Hh    ��      p�               �L"    , �,  8         $     "    �L        � f  
  
�    
�H T   %              �     }        �GG %              
"   
 $ 
"   
   
"   
 M
"   
 M(�  L ( l       �        ,i    ��    � P   �        8i    �@    
� @  , 
�       Di    ��    Mp�               �L
�    %              � 8      Pi    � $         �     M     
�    � :     
"   
 �p� @  , 
�       `j    �� ?   �p�               �L
"   
 , 
"   
   p� @  , 
�       �j    ��      p�               �L"    , 
"   
  p� @  , 
�       k    �� �    p�               �L"    ,     "    _� �    �%L C <   OPEN QUERY Query-Main FOR EACH FacCPedi NO-LOCK INDEXED-REPOSITION.     "    CP� �   K ((        "    PO%                   "    �� �     "    M (   "           "    �%              @ �,  8         $     "    M        � �    
�    p�,  8         $     � �   b        � �   M
�    %               �    "      � z         %              %                   "      %                  "      "      "     T(        "    b%              "    b� z   �"      �       "    M�    "    b� 8   �� �      � 8   M�    "     � 8    S    "      "    �    "    `%                � @    �     t T     P   4       �"      (0       4       b"      � �      � �    M� x   bT ,  %              T   "    b"    �� z     � 8   M� x   bT    �    "    b� 8   �"      � 8   M"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "    M%              � �    �� �     4  `     "      
�H T   %              �     }        �GG %              
"   
 M
"   
 �
"   
 M
"   
   (�  L ( l       �        �q    ��    � P   �        �q    �@    
� @  , 
�       �q    ��    Mp�               �L
�    %              � 8      �q    � $         �            
�    � :   M
"   
 �p� @  , 
�       �r    �� $  
 �p�               �L"    ,       "  
  ^�    � �    b� z   �      "  	    �    � �  ~ �� z   b�   � x     � z     � �    M�   � x     � z   M� �  ~ b�   � x     � z     � �  ~   
�H T   %              �     }        �GG %              
"   
 �
"   
 M
"   
 �
"   
 �(�  L ( l       �        Ht    ��    � P   �        Tt    �@    
� @  , 
�       `t    ��    �p�               �L
�    %              � 8      lt    � $         �            
�    � :     
"   
 �p� @  , 
�       |u    �� �   �p�               �L"    , 
"   
   p� @  , 
�       �u    �� �     p�               �L"    , 
"   
  p� @  , 
�       ,v    �� d    p�               �L"    ,     %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    � �      � z         "  	  `�     "    `T    "      "      @ A,    �   � x   �� �     "    M"       T      @   "    �(        "      � �    M� �      � x   M"    `     "  	   %              D H   @ A,    �   � x   M� �     "    M"    G,    S   "    M� �    G� z   �%                T      @   "    �(        "      � �    M� �      � x   M"    F     "  
   %                         "    �� �     "    M           "      � �   M"      
�H T   %              �     }        �GG %              
"   
 F
"   
   
"   
 F
"   
 M(�  L ( l       �        Hz    ��    � P   �        Tz    �@    
� @  , 
�       `z    ��    Fp�               �L
�    %              � 8      lz    � $         �     M     
�    � :   �
"   
 �p� @  , 
�       |{    �� �   �p�               �L"    , 
"   
   p� @  , 
�       �{    �� d     p�               �L"    , "      %               �     }        �%              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    �%     start-super-proc ��%     adm2/data.p %     start-super-proc ��%     adm2/dataext.p %     start-super-proc ��%     adm2/dataextcols.p %     start-super-proc ��%     adm2/dataextapi.p `
�H T   %              �     }        �GG %              
"   
 M
"   
 �
"   
 M
"   
 M(�  L ( l       �            ��    � P   �             �@    
� @  , 
�       ,    ��    Mp�               �L
�    %              � 8      8    � $         �     M     
�    � :   M
"   
 �p� @  , 
�       H�    �� �   �p�               �L%               %     "dConPedCre.i"  
�H T   %              �     }        �GG %              
"   
 M
"   
 �
"   
 M
"   
   (�  L ( l       �        �    ��    � P   �        �    �@    
� @  , 
�       (�    ��    Mp�               �L
�    %              � 8      4�    � $         �            
�    � :   M
"   
 �p� @  , 
�       D�    �� �   �p�               �L"    , 
�     	        �G
�H T   %              �     }        �GG %              
"   
 M
"   
 �
"   
 M
"   
   (�  L ( l       �        ��    ��    � P   �        �    �@    
� @  , 
�       �    ��    Mp�               �L
�    %              � 8       �    � $         �            
�    � :   M
"   
 �p� @  , 
�       0�    �� �  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 M
"   
 �
"   
 M
"   
   (�  L ( l       �        �    ��    � P   �        �    �@    
� @  , 
�        �    ��    Mp�               �L
�    %              � 8      �    � $         �            
�    � :   M
"   
 �p� @  , 
�       �    �� �  	 �p�               �L
"   
 , 
"   
 �     � �  	   �        t�    �
�H T   %              �     }        �GG %              
"   
 M
"   
 �
"   
 M
"   
   (�  L ( l       �        �    ��    � P   �         �    �@    
� @  , 
�       �    ��    Mp�               �L
�    %              � 8      �    � $         �            
�    � :   M
"   
 �p� @  , 
�       (�    ��     �p�               �L"    , 
"   
   �       ��    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 M
"   
 �
"   
 M
"   
   (�  L ( l       �        �    ��    � P   �        �    �@    
� @  , 
�       $�    ��    Mp�               �L
�    %              � 8      0�    � $         �            
�    � :   M
"   
 �p� @  , 
�       @�    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 G        � �   M
�    
�             �Gp�,  8         $     
"   
 G        � �   M
�    �    � �     
�        "    F� �    �%     modifyListProperty 
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � u     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           �   l       ��                 [    �               ��_                    O   ����    e�          O   ����    R�          O   ����    ��        $  j  �   ���                       �U     
                    � ߱              k  (  �      V      4   ����V                �                      ��                  l  ~                  h}a                       l  8  �  �  m  PV            o  �  `      �V      4   �����V                p                      ��                  p  }                  �}a                       p  �  �  o   q      ,                                 �  �   r  �V      �  �   s  �V      $  $  t  �  ���                        W     
                    � ߱        8  �   u  @W      L  �   v  `W      `  �   y  �W          $   |  �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               <�`                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  ��a                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �   �  �X          O   �  ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��            	     J  �  �                �`                    O   ����    e�          O   ����    R�          O   ����    ��        $  j  �   ���                       `a                         � ߱        �  $  k  8  ���                       �a                         � ߱        �a     
                b  @         �a              � ߱        D  $   �  d  ���                         T      �  x                      ��        0         �  �                  ��F      8c     L     �  �      $  �  �  ���                       �b                         � ߱          $  �  �  ���                       �b                         � ߱            4   ����c  Xc     
                xc                     �c                         � ߱          $  �    ���                                                            ��                  �  �                  �F                �     �  �  �  $  �  L  ���                       td       !       !           � ߱          �      L  �                      ��        0         �  �                  �E     ( �d            �  x      $  �     ���                       �d       (       (           � ߱        �  $  �  x  ���                       �d       (       (           � ߱            4   �����d        �  �  L      e      4   ����e                \                      ��                  �  �                  <YG                       �  �  �  $  �  �  ���                       �e       !       !           � ߱            O   �  �� ��          $  �  �  ���                       �e                         � ߱        hf     
                �f                     4h  @        
 �g          �h  @        
 Th          �h                     �h     
                \i                     �j  @        
 lj          k  @        
 �j          \k  @        
 k              � ߱        x  V   �  $  ���                        P	    ~  �  $	      hk      4   ����hk  �k                     �k                     �k                     Tl                         � ߱            $    �  ���                       �	    �  l	  |	      �l      4   �����l      �   �  �l      �	  $  �  �	  ���                       m                         � ߱        �
  $  �  
  ���                       (m                         � ߱          �
                              ��        0         �  �                  44G      �m     �     �  @
      $  �  �
  ���                       Hm                         � ߱        l  $  �  @  ���                       xm                         � ߱            4   �����m  �m                     n                      n                     pn                     �n                         � ߱        D  $  �  |  ���                             �  `  p      �n      4   �����n      $  �  �  ���                       �n          p             � ߱        �  $  �  �  ���                       p                         � ߱          �      �  \                      ��        0         �  �                  ,5G      �p          �         $  �  �  ���                       $p                         � ߱        L  $  �     ���                       Tp                         � ߱            4   ����|p      $  �  �  ���                       �p                         � ߱        8q     
                �q                     s  @        
 �r              � ߱        �  V   �  �  ���                        s       
       
       Ds       	       	       xs                     �s                         � ߱        �  $    D  ���                       �  $  �    ���                       �s                         � ߱        �s     
                xt                     �u  @        
 �u           v  @        
 �u          xv  @        
 8v              � ߱        |  V   �  H  ���                          �      �  \                      ��        0    	     (  =                  �{E      w     4     (        $  (  �  ���                       �v                         � ߱        <  $  (    ���                       �v                         � ߱        L  4   �����v      4   ����w  �  $  -  �  ���                       |w                         � ߱        �    /  �  L      �w      4   �����w                �                      ��                  0  4                  0|E                       0  �  �w                     Hx       	       	           � ߱            $  1  \  ���                             6  �  h      px      4   ����px  	              �                      ��             	     8  <                  �|E                       8  �  y                     ly       
       
           � ߱            $  9  x  ���                       �y                     �y                         � ߱        �  $  C  �  ���                       �y     
                xz                     �{  @        
 �{           |  @        
 �{              � ߱            V   Q  `  ���                                    J `          �  �  � X@                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
             
                                                                                                                                                                                                                               ) �   �   �   �       (  8  H  X  h  x  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX     ) �   �   �   �      (  8  H  X  h  x  �  �  �  �  �   �     (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX  �   :                  � �                     �    ��                      ��                            ����                            c                          b�                                �   l       ��                      �               �_G                    O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   l       ��                  !  0  �               �bG                    O   ����    e�          O   ����    R�          O   ����    ��      7       �              �                  $                  d  /  -  $     4  �                      3   ������            T                      3   �����      O   .  ��  ��   �               �          �  �    �                                             ��                            ����                                            H          �   l       ��                  :  e  �               �E                    O   ����    e�          O   ����    R�          O   ����    ��      V       �              �                $                  `       ,             �          k                                �  /  Y  t     �  H�                      3   ����$�            �                      3   ����P�     /  [  �     �  x�                      3   ����\�  x                             3   ������      $   [  L  ���                                                   � ߱                  �  �                  3   ������      $   [  �  ���                                                   � ߱        X  $  _  ,  ���                       ��                         � ߱            O   c  ��  ��  ��               �          �  �   @ �                                                              0              0           ��                            ����                                                       �   l       ��                  o  �  �               d+G                    O   ����    e�          O   ����    R�          O   ����    ��      �       $                  `                    �          k                      �              /  �  L     \  �                      3   ����Ȍ  �        |  �                  3   �����      $   �  �  ���                                                   � ߱                                      3   ������      $   �  @  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   l       ��                      �               0�F                    O   ����    e�          O   ����    R�          O   ����    ��              �   �       �      4   �����      �     ,�    ��                            ����                            TXS appSrvUtils FacCPedi Pedidos al Credito O:\on_in_co\APLIC\vta\dConPedCre.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "dConPedCre.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH FacCPedi NO-LOCK INDEXED-REPOSITION ,   hQuery cOpenQuery cDBNames cPhysicalTables cKeyTableEntityFields cKeyTableEntityValues cKeyTableEntityMnemonic cKeyTableEntityObjField cDBName cEntityFields lHasObjectField lHasAudit lHasComment lHasAutoComment iLookup iAlias  STATIC setDBNames OPEN QUERY Query-Main FOR EACH FacCPedi NO-LOCK INDEXED-REPOSITION.  FOR   PRESELECT  setOpenQuery 5 showMessage ; CodCia CodDiv CodDoc NroPed CodCli NomCli FchPed fchven FchEnt FchAprobacion FlgEst FlgSit ImpTot CodMon CodVen FmaPgo usuario Query-Main INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p CodCia CodDiv CodDoc NroPed CodCli NomCli FchPed fchven FchEnt FchAprobacion FlgEst FlgSit ImpTot CodMon CodVen FmaPgo usuario RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI qDataQuery   82  @  x@      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   M	  e	  g	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props j  k  l  m  o  p  q  r  s  t  u  v  y  |  }  ~              P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �  �  �           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable   �        |     cQueryString    �        �  
   hQuery  �        �  
   hBuffer �        �     cOpenQuery          �     cDBNames    (             cPhysicalTables T        <     cKeyTableEntityFields   �        h     cKeyTableEntityValues   �        �     cKeyTableEntityMnemonic �         �     cKeyTableEntityObjField �     !   �     cDBName      "        cEntityFields   <     #   ,     lHasObjectField \     $   P     lHasAudit   |     %   p     lHasComment �     &   �     lHasAutoComment �     '   �     iLookup        (   �     iAlias  |    3   Y   �                            initProps   j  k  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �    �  �  (  -  /  0  1  4  6  8  9  <  =  C  Q  �            �     lRet                      piTableIndex    �  h  *   Z   �  �      T                  deleteRecordStatic  �  �  �  �  �  �  �  �           <  =  Y  Z  v  w  �  �  �  �  �  �  �  �      $  %  A  B  ^  _  {  |  �  �  �  �  �  �                 !       $  l     [             X                  pushRowObjUpdTable    �        �        pcValType                  $       (  �     \       p      �                  pushTableAndValidate    -  .  0  $                pcContext   <             $       `        T        pcMessages            x        pcUndoIds   �  �     ]              �                  remoteCommit    Y  [  _  c  e  �             $                       pcMessages            ,        pcUndoIds   �  x     ^       �      h                  serverCommit    �  �  8  �     _               �                  getRowObjUpdStatic  �  �  �       `               �                  disable_UI      �  �#  
     ,      4#                      (  P  \     RowObject   d         l         t         |         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �                                    CodCia  CodDiv  CodDoc  NroPed  CodCli  NomCli  FchPed  fchven  FchEnt  FchAprobacion   FlgEst  FlgSit  ImpTot  CodMon  CodVen  FmaPgo  usuario RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     8  D     RowObjUpd   X         `         h         p         x         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �                                    CodCia  CodDiv  CodDoc  NroPed  CodCli  NomCli  FchPed  fchven  FchEnt  FchAprobacion   FlgEst  FlgSit  ImpTot  CodMon  CodVen  FmaPgo  usuario RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   L          @  
   appSrvUtils t       `     xiRocketIndexLimit  �        �  
   gshAstraAppserver   �        �  
   gshSessionManager   �        �  
   gshRIManager            �  
   gshSecurityManager  8        $  
   gshProfileManager   d  	 	     L  
   gshRepositoryManager    �  
 
     x  
   gshTranslationManager   �        �  
   gshWebManager   �        �     gscSessionId    �        �     gsdSessionObj                
   gshFinManager   D         4   
   gshGenManager   h         X   
   gshAgnManager   �         |      gsdTempUniqueID �         �      gsdUserObj  �         �      gsdRenderTypeObj    �         �      gsdSessionScopeObj  !       !  
   ghProp  8!       ,!  
   ghADMProps  \!       L!  
   ghADMPropsBuf   �!       p!     glADMLoadFromRepos  �!       �!     glADMOk �!       �!  
   ghContainer �!    	   �!     cObjectName �!    
   �!     iStart  "       "     cAppService <"       0"     cASDivision h"       P"     cServerOperatingMode    �"       |"     cContainerType  �"       �"     cQueryString    �"       �"  
   hRowObject  �"       �"  
   hDataQuery  #       #     cColumns             $#     cDataFieldDefs  P#       D#  FacCPedi    l#    X  `#  RowObject         X  |#  RowObjUpd            9   �   �   �   �           5  A  B  C  E  G  H  I  M  N  Q  R  S  T  V  X  Z  \  ]  ^  a  c  d  f  g  h  i  j  p  r  x  z  |  }  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  >
  ?
  A
  B
  C
  D
  E
  F
  H
  I
  J
  K
  L
  M
  N
  O
  P
  Q
  R
  S
  T
  U
  V
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
  p
  q
  r
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
                     	  
  {  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  *  �  �  �  �  �  �  �  �  �  �    -  L  N  c  �        .  /  0  3  4  5  <  =  Z  n  �     !  -  Q  �  �  �  �  �  >  �  �  �  �  �  �  �  >  X  b  |  �  �  �  �  �  �  �  �    (  2  L  n  y  z      ��  C:\Progress\OpenEdge\src\adm2\data.i �'  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    �'  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i  (  �� , O:\on_in_co\APLIC\vta\dConPedCre.i   T(  �:  C:\Progress\OpenEdge\src\adm2\query.i    �(  z + C:\Progress\OpenEdge\src\adm2\delrecst.i �(  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  �(  F� ) C:\Progress\OpenEdge\gui\fnarg    )   ( %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   L)  �   C:\Progress\OpenEdge\src\adm2\dataquery.i    �)  �Z ' %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   �)  �< ! C:\Progress\OpenEdge\src\adm2\appserver.i    *  �� & %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   @*  I� " C:\Progress\OpenEdge\src\adm2\smart.i    �*  Ds % C:\Progress\OpenEdge\gui\fn  �*  tw $ %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �*  Q. # C:\Progress\OpenEdge\gui\set  +  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i H+  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    |+  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    �+  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  ,  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i 8,  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i x,   	 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    �,  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   �,  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   <-  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �-  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �-  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �-  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i @.  �j  C:\Progress\OpenEdge\gui\get t.  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �.  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �.  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i $/  Su  C:\Progress\OpenEdge\src\adm2\globals.i  X/  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �/  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �/  �  C:\Progress\OpenEdge\src\adm2\appsprto.i 0  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   D0  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    �0  ª 
 %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   �0  ��  C:\Progress\OpenEdge\src\adm2\qryprto.i  1  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   D1  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i �1  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    �1  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i    2  �   O:\on_in_co\APLIC\vta\dConPedCre.w       �   �      h2  [  b     x2     `  %   �2  �   �     �2     �  .   �2  �   x     �2     Y     �2  �   V     �2     4  #   �2  �   2     �2       #   3  �        3     �  #   (3  �   �     83     �  #   H3  �   �     X3     �  #   h3  �   �     x3     ~  #   �3  �   |     �3     Z  #   �3  �   X     �3     6  #   �3  �   )     �3       -   �3  �        �3     �  ,   4  k   �     4  �  �     (4     �  +   84  �  �     H4     ~  +   X4  �  {     h4     a  +   x4  �  ^     �4     D  +   �4  �  A     �4     '  +   �4  �  $     �4     
  +   �4  �       �4     �  +   �4  �  �     5     �  +   5  �  �     (5     �  +   85  �  �     H5     �  +   X5  �  �     h5     y  +   x5  �  v     �5     \  +   �5  �  Y     �5     ?  +   �5  �  <     �5     "  +   �5  �       �5       +   �5  �       6     �  +   6  �  �     (6     �  +   86  �  �     H6     �  +   X6  �  �     h6     l  #   x6  �  k     �6     I  #   �6  k  $     �6       #   �6  j       �6     �  #   �6  i  �     �6     �  #   �6  _  �     7     �  *   7  ^  �     (7     e  *   87  ]  d     H7     >  *   X7  \  =     h7       *   x7  [       �7     �  *   �7  Z  �     �7     �  *   �7  Y  �     �7     �  *   �7  X  �     �7     {  *   �7  W  z     8     T  *   8  V  S     (8     -  *   88  U  ,     H8       *   X8  T       h8     �  *   x8  S  �     �8     �  *   �8  R  �     �8     �  *   �8  Q  �     �8     j  *   �8  P  i     �8     C  *   �8  O  B     9       *   9  N       (9     �  *   89  @  �     H9     �  #   X9  	  �     h9     �  )   x9  �   {     �9     Y  #   �9  �   X     �9     6  #   �9  �   5     �9       #   �9  �        �9     �  #   �9  �   �     :     �  #   :  �   �     (:     �  #   8:  �   :     H:     �  (   X:  g   �     h:  a   �      x:     m  '   �:  _   k      �:     I  #   �:  ]   G      �:     %  #   �:  I         �:  �     !   �:     �  &   �:  �   �  !   ;     �  #   ;  �   �  !   (;     f  #   8;  �   d  !   H;     B  #   X;  g   (  !   h;     	     x;  O   �  !   �;  �   {  "   �;     y  %   �;  �   I  "   �;     �  $   �;  �   �  "   �;     �  #   �;  �   �  "   �;     �  #   <  �   �  "   <     ~  #   (<  �   }  "   8<     [  #   H<  �   G  "   X<     %  #   h<  }     "   x<     �  #   �<     {  "   �<     -  !   �<     �      �<          �<     3     �<  �   *     �<  O        �<          =     �     =  �   �     (=  �   {     8=  O   m     H=     \     X=          h=  y   �
     x=  �   �
  	   �=  G   �
     �=     �
     �=     v
     �=  c   
  	   �=  x   
     �=  M   �	     �=     �	     �=     �	     >  a   �	     >  �  d	     (>     E	     8>  �  	     H>  O   	     X>     �     h>     �     x>  �   �     �>     �     �>     �     �>  x   �     �>     �     �>     `     �>     \     �>     H     �>     /     ?  Q        ?     �     (?     �     8?     y     H?     _     X?  ]   Y  	   h?     O     x?       	   �?     �  
   �?     �  	   �?  Z   �     �?     �     �?     �     �?     �     �?     �     �?  c   c     @     A     @     �      (@     �      8@     �      H@     �      X@     !       h@           