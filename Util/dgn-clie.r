	��V�8�K�6  � �                                              o' 36F800EFutf-8 MAIN O:\on_in_co\Util\dgn-clie.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,NomCli character 0 0,DirCli character 1 0,NroCard character 2 0,Ruc character 3 0,CodDept character 4 0,RowNum integer 5 0,RowIdent character 6 0,RowMod character 7 0,RowIdentIdx character 8 0,RowUserProp character 9 0,ChangedFields character 10 0     �               �             �� �   L�              4�              |>     +   � �  W   �� `  X   �   Y   0�   [   @�   \   X� <  ]   ��    ^   �� 0  `   ? ��   iSO8859-1                                                                           �    �                                      �                   �                       @   ��   T�              ��  �   d       p           �                                             PROGRESS                         �           
    
                    �              �                                                                                                     
               INTEGRAL                         PROGRESS                                  �                                �)�K               ߿                              �  l                      �  |  u=     CODCLINOMCLIDIRCLIRUCTPOCLILOCCLIFAXCLIFCHINGFLGSITCODPOSCODCIAREFERENCIASCODPAISCODDEPTCODPROVCODDISTGIRCLICNDVTATELFNOSE-MAILFCHACTCLFCLIUSUARIOREPLEGFNREPRTRANSPORTEMONLCIMPLCCODVENDIRENTFCHCESCONTACFCHVLCCODANTUSRLCVENANTFLAGAUTUSRAUTOBSAUTCODDIVDEPENTPROENTDISENTLINCREUSRCRETOTUSOFCHAUTAVAL1AVAL2TLFLOGJFELOGDNIRUCOLDCANALCLFCOMNROCARDDIRREFCODUNICOAPEPATAPEMATNOMBRE                                                                         	          
                                                                                                                                                                                                                                    !          "          #          $          %          &          '          (          )          *          +          ,          -         .         /          0         1         2         3          4         5          6          7          8          9          :          ;          <          =          >          |  �      �  
    
                  �  �             h                                                                                          �          
  (        �  
    
                  �  X                                                                                                                 
  �  "      P  
    
                  <  	             �                                                                                          "          
  �	  /      �  
    
                  �  �	             l	                                                                                          /          
  ,
  B      �	  
    
                  �	  \
             
                                                                                          B          
  �
  T      T
  
    
                  @
    	           �
                                                                                          T          
  �  i         
    
                  �
  �  
           p                                                                                          i          
  0        �  
    
                  �  `                                                                                                                 
  �  �      X                         D               �                                                                                          �            �  �                              �  �             t                                                                                          �            4  �      �  
    
                  �  d                                                                                                        �          
  �  �      \  
    
                  H               �                                                                                          �          
  �  �        
    
                  �  �             x                                                                                          �          
  8  �      �                        �  h             $                                                                                          �            �  �      `                        L               �                                                                                          �            �  �                              �  �             |                                                                                          �                �      �                        �  <             (                                                                                          �            �         �       �  X  �     �  �  @G      $  
       �             ,                �              �       �  X  8     T  �        �         �         �    L          <      �                 h�                                               l�          �  �  L lt                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                   �  �  �                           $  ,  4  L  @          P             h  p  x  �  �                         �  �  �  �  �          �             �  �  �     �                                                             $  0  8  D                              H  P  X  `                             d  p  x  �                             �  �  �  �                                                                          NomCli  x(50)   Nombre  Nombre      Nombre del Cliente  DirCli  x(60)   Direcci�n   Direcci�n       Direcci�n del Cliente   NroCard x(8)    NroCard Nrocard     Ruc x(11)   Ruc Ruc     Registro Unico de Contribuyente (Cliente)   CodDept X(3)    Departamento    Departamento        RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  ���������               �        �        �                �     i     i     i     		 	
 	    z  �  �  �  �  �  �  �  �  �                                                                                                                                     	                  
                                                                     L  T  \  l  d          p             �  �  �  �  �          �             �  �  �  �  �                         �  �  �     �                       0  8  @  `  P                         d  l  x  �                             �  �  �  �                              �  �  �  �                             �  �  �  �                             �  �  �                                    $  4                                                                          NomCli  x(50)   Nombre  Nombre      Nombre del Cliente  DirCli  x(60)   Direcci�n   Direcci�n       Direcci�n del Cliente   NroCard x(8)    NroCard Nrocard     Ruc x(11)   Ruc Ruc     Registro Unico de Contribuyente (Cliente)   CodDept X(3)    Departamento    Departamento        RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  ���������               �        �        �                �     i     i     i     		 	
 	    z  �  �  �  �  �  �  �  �  �  �    ��                            ����                                 ��                    `h       ��                    c�    undefined                                                               �       ��  �   l   ��  ��                    �����               |�d	                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       �     9          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   �f	                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  N  Q  L              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  S  Y  �              �e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                  [  \  p              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  ^  a  p              t�f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  c  e  �              D�f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  g  j  �	              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  l  m  H              �e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  o  p  T              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  r  t  T              , f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                  v  w  |              H�d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  y  z  |              P^e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  |  }  |              �^e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                    �  |              p_e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              Ȝf	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �               �f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              �|f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              $}f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �               �f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `              pke	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d              xre	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �              �f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              �{e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              �{e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              ��d	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              �e	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                  �  �  @$              f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                  �  �  l%              4Le	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                  �  �  d'              P�d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                  �  �  h(              �#f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                  �  �  �)              4Nf	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                  �  �  @+              LZf	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                  �  �  �,              @�e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                       �-              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                      �/              ��d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                  
    �0              T�e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                      �1              l�d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                      �2               �d	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3     g       CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	 n       CHARACTER,  canNavigate �3      �3      (4    x       LOGICAL,    closeQuery  4      44      `4   
 �       LOGICAL,    columnProps @4      l4      �4    �       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	 �       CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7    �       LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7    �       LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7    �       CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8    �       CHARACTER,  hasForeignKeyChanged    88      d8      �8          LOGICAL,    openDataQuery   |8      �8      �8    %      LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	 3      LOGICAL,    prepareQuery    9      49      d9    =      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    J      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 W      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	 a      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	 k      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;    u      CHARACTER,  assignDBRow                             <  �;      ��                  �  �  <              p�f	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                  �    L=              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                      �>              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                    
  �?              �ve	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                       A              lf	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                      PB              (�e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                      PC              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                      PD              �d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                      PE              �d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                      PF              ܚe	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                    !  \G              ԛe	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  #  $  �H              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  &  (  �I              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  *  +  �J              �f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  -  .  �K              PTf	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  0  3  �L              �Tf	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M    �      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP    �      CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P          CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q           CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !  +      CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "  <      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  K      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  Z      CHARACTER,  getForeignValues    @R      lR      �R  %  i      CHARACTER,  getQueryPosition    �R      �R      �R  &  z      CHARACTER,  getQuerySort    �R      �R      S  '  �      CHARACTER,  getQueryString  �R      (S      XS  (  �      CHARACTER,  getQueryWhere   8S      dS      �S  )  �      CHARACTER,  getTargetProcedure  tS      �S      �S  *  �      HANDLE, indexInformation    �S      �S      T  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .  �      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /  
      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0        CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1  '      CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2  7      LOGICAL,    removeQuerySelection    �W      �W      (X  3  H      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  ]      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
 k      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6  v      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7  �      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8  �      LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\              x�d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                  �  �  �]               �d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                  �  �  �^              �	g	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                  �  �  `              t
g	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                  �  �  a              �g	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                  �  �  b              �g	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                  �  �  Dc              �g	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                  �  �  Hd              �g	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <  �      CHARACTER,  getASBound  �d      e      0e  = 
 �      LOGICAL,    getAsDivision   e      <e      le  >  �      CHARACTER,  getASHandle Le      xe      �e  ?  �      HANDLE, getASHasStarted �e      �e      �e  @        LOGICAL,    getASInfo   �e      �e      f  A 	       CHARACTER,  getASInitializeOnRun    �e       f      Xf  B        LOGICAL,    getASUsePrompt  8f      df      �f  C  4      LOGICAL,    getServerFileName   tf      �f      �f  D  C      CHARACTER,  getServerOperatingMode  �f      �f      g  E  U      CHARACTER,  runServerProcedure  �f      $g      Xg  F  l      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G        LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              d�g	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl              0yf	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              �g	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o              Ծg	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p              �h	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              8h	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              �h	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              h	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t              �h	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u              ��g	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                  �  �  �v              ��g	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                  �  �  �w              D�g	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                  �  �  �x              |�g	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                  �  �  |z              �Af	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                  �  �  �{              lAh	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                  �  �  �|              H=h	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                  �  �  x~              LXh	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                  �  �  �              4Rh	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                  �  �  ��              �jh	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                  �     L�              <Lh	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                      t�              �dh	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
 L      LOGICAL,    assignLinkProperty  ؃      �      8�  P  W      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  Q  j      CHARACTER,  getChildDataKey ��      ̄      ��  R  x      CHARACTER,  getContainerHandle  ܄      �      <�  S  �      HANDLE, getContainerHidden  �      D�      x�  T  �      LOGICAL,    getContainerSource  X�      ��      ��  U  �      HANDLE, getContainerSourceEvents    ��      ��      ��  V  �      CHARACTER,  getContainerType    ܅      �      <�  W  �      CHARACTER,  getDataLinksEnabled �      H�      |�  X  �      LOGICAL,    getDataSource   \�      ��      ��  Y  �      HANDLE, getDataSourceEvents ��      ��      �  Z        CHARACTER,  getDataSourceNames  Ԇ       �      4�  [  !      CHARACTER,  getDataTarget   �      @�      p�  \  4      CHARACTER,  getDataTargetEvents P�      |�      ��  ]  B      CHARACTER,  getDBAware  ��      ��      �  ^ 
 V      LOGICAL,    getDesignDataObject ȇ      �      (�  _  a      CHARACTER,  getDynamicObject    �      4�      h�  `  u      LOGICAL,    getInstanceProperties   H�      t�      ��  a  �      CHARACTER,  getLogicalObjectName    ��      ��      ��  b  �      CHARACTER,  getLogicalVersion   Ј      ��      0�  c  �      CHARACTER,  getObjectHidden �      <�      l�  d  �      LOGICAL,    getObjectInitialized    L�      x�      ��  e  �      LOGICAL,    getObjectName   ��      ��      �  f  �      CHARACTER,  getObjectPage   ̉      ��      (�  g  �      INTEGER,    getObjectParent �      4�      d�  h        HANDLE, getObjectVersion    D�      l�      ��  i        CHARACTER,  getObjectVersionNumber  ��      ��      �  j  %      CHARACTER,  getParentDataKey    Ċ      ��      $�  k  <      CHARACTER,  getPassThroughLinks �      0�      d�  l  M      CHARACTER,  getPhysicalObjectName   D�      p�      ��  m  a      CHARACTER,  getPhysicalVersion  ��      ��      �  n  w      CHARACTER,  getPropertyDialog   ȋ      �      (�  o  �      CHARACTER,  getQueryObject  �      4�      d�  p  �      LOGICAL,    getRunAttribute D�      p�      ��  q  �      CHARACTER,  getSupportedLinks   ��      ��      ��  r  �      CHARACTER,  getTranslatableProperties   ��      �      (�  s  �      CHARACTER,  getUIBMode  �      4�      `�  t 
 �      CHARACTER,  getUserProperty @�      l�      ��  u  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    |�      č      ��  v  	      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ܍      $�      P�  w  	      CHARACTER,INPUT pcLink CHARACTER    linkProperty    0�      t�      ��  x  #	      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      ��      �  y  0	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      x�      ��  z  <	      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ̏      ��  {  J	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ܏      $�      T�  |  W	      CHARACTER,  setChildDataKey 4�      `�      ��  }  f	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  p�      ��      �  ~  v	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ̐      �      @�    �	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents     �      `�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled |�      ��      ��  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ԑ      �      L�  �  �	      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ,�      l�      ��  �  �	      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      Ȓ      ��  �  �	      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ܒ      $�      T�  �  �	      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 4�      x�      ��  �  
      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Г      ��  � 
  
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ܓ      �      P�  �  +
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    0�      x�      ��  �  ?
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      Ȕ       �  �  P
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      $�      \�  �  f
      LOGICAL,INPUT c CHARACTER   setLogicalVersion   <�      x�      ��  �  {
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Е       �  �  �
      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��       �      P�  �  �
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    0�      p�      ��  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ̖       �  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      (�      \�  �  �
      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   <�      |�      ��  �  �
      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ԗ      �  �  �
      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      ,�      \�  �  
      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   <�      ��      ��  �        LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ܘ      �  �  ,      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      <�      h�  � 
 F      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  Q      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �  a      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	 m      CHARACTER,INPUT pcName CHARACTER    l�      ��  0�      �       4   �����                 @�                      ��                    G                  T�g	                         Ě          \�  ؛      �       4   �����                 �                      ��                    F                  ��g	                         l�  �    3  �  ��      �       4   �����                 ��                      ��                  ?  A                  \�g	                       ?  �         @                                  ,     
                    � ߱        �  $  C  ��  ���                           $  E  @�  ���                       x                         � ߱        x�    K  ��  �      �      4   �����                �                      ��                  L  	                  �g	                       L  ��  H�  o   O      ,                                 ��  $   P  t�  ���                       �  @         �              � ߱        ��  �   Q        Ȟ  �   R  �      ܞ  �   T        �  �   V  x      �  �   X  �      �  �   Z  `      ,�  �   [  �      @�  �   \        T�  �   _  �      h�  �   a         |�  �   b  |      ��  �   d  �      ��  �   e  t      ��  �   f  �      ̟  �   g  ,      ��  �   h  �      ��  �   n  �      �  �   p  P	      �  �   v  �	      0�  �   x   
      D�  �   z  t
      X�  �   {  �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  7	  e	  ��              4�g	                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        h�  $ K	  آ  ���                           O   c	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ  u                     8�    �	  ��  �      t      4   ����t                 �                      ��                  �	  
                  �0h	                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    <
  T�  Ц      x      4   ����x                �                      ��                  =
  �
                  ddg	                       =
  d�  ��  �   ?
  �      �  �   @
  T      �  �   A
  �      0�  �   B
  D      D�  �   C
  �      X�  �   D
  �      l�  �   F
  p      ��  �   G
  �      ��  �   H
  X      ��  �   I
  �      ��  �   J
  �      Ч  �   K
  D       �  �   L
  �       ��  �   M
  �       �  �   N
  x!       �  �   O
  �!      4�  �   P
  h"      H�  �   Q
  �"      \�  �   R
  `#      p�  �   S
  �#      ��  �   T
  X$      ��  �   U
  �$      ��  �   V
  �$      ��  �   W
  L%      Ԩ  �   X
  �%      �  �   Y
  <&      ��  �   Z
  �&      �  �   [
  4'      $�  �   \
  �'      8�  �   ]
  ,(      L�  �   ^
  h(      `�  �   `
  �(      t�  �   a
  X)      ��  �   b
  �)      ��  �   c
  *      ��  �   d
  �*      ĩ  �   e
  �*      ة  �   f
  l+      �  �   g
  �+       �  �   h
  \,      �  �   i
  �,      (�  �   j
  L-      <�  �   k
  �-      P�  �   l
  <.      d�  �   m
  �.      x�  �   n
  4/      ��  �   o
  �/          �   p
  $0      d�    �
  ��  8�      T0      4   ����T0                H�                      ��                  �
  y                  Xgg	                       �
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
  �7      ��  �   �
  <8      ��  �   �
  �8      Ĭ  �      ,9      ج  �     �9      �  �     :       �  �     X:      �  �     �:      (�  �     H;      <�  �     �;      P�  �     8<          �     �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  (                  ��f	                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �    �  ��  ��      K      4   ����K      /   �  а     �                          3   ����K             �                      3   ����<K  Զ    �  ,�  ��  �  XK      4   ����XK  
              ��                      ��             
     �  J                  h	                       �  <�  ̱  �   �  �K      $�  $  �  ��  ���                       �K     
                    � ߱        8�  �   �  L      ��  $   �  d�  ���                       ,L  @         L              � ߱        L�  $  �  ��  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱        ܳ  V   �  �  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        l�  $    x�  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   +  �  ���                                      ̵                      ��                  L  �                  ph	                       L  ��  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   a  �  ���                        adm-clone-props �  ��              �     W     `                          \  n                     start-super-proc    �  d�  �           �     X                                  �                     l�      �   �      �X      4   �����X      /     ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $    ��  ���                       @Y                         � ߱        ��    ,  �  \�  ��  \Y      4   ����\Y                и                      ��                  -  1                   �g	                       -  �  pY                     �Y                     �Y                         � ߱            $  .  l�  ���                             2  �  T�      �Y      4   �����Y  �Y                         � ߱            $  3  (�  ���                       |�    :  ��  ��  �  �Y      4   �����Y      $  ;  ع  ���                       Z                         � ߱            �   X  Z      XZ     
                �Z                     $\  @        
 �[              � ߱        ��  V   l  �  ���                        ��  �   �  0\      ��      غ  �      p\      4   ����p\      /     �     $�                          3   �����\            D�                      3   �����\  �\     
                <]                     �^  @        
 L^              � ߱        H�  V   +  T�  ���                        �^     
                T_                     �`  @        
 d`              � ߱        t�  V   O  �  ���                        ��    �  ��  �      �`      4   �����`                �                      ��                  �  �                  d�g	                       �  ��  ��  /   �  H�     X�                          3   �����`            x�                      3   �����`      /   �  ��     Ľ                          3   ����a            �                      3   ����$a   �  /  <   �         Xa                      3   ����@a  initProps   x�  0�                   Y     �             �          �    	                                   t�          �  �      ��                �  �  4�               9Y	                    O   ����    e�          O   ����    R�          O   ����    ��                            L�          ��  p   �  (|  ��      �  ��  �     4|                �                      ��                  �  �                  t9Y	                       �  ��  4�  :  �                 $  �  `�  ���                       H|                         � ߱        �  �     `|                                        ��                  �  �                  |�X	                       �  ��  ��  ��     t|                                        ��                  �                     D�X	                       �  (�  0�   �     �|                                        ��                                      �X	                         ��  ��  ��     �|                                        ��                    :                  ��W	                         @�  H�  8�     �|                                        ��                  ;  W                  ��W	                       ;  ��  ��  ��     �|                                        ��                  X  t                  t�W	                       X  X�  `�  P�     �|                                        ��                  u  �                  D�W	                       u  ��  ��  ��     �|  	                                      ��             	     �  �                  �W	                       �  p�  x�  h�      }  
                                      ��             
     �  �                  �W	                       �  ��  �  ��     }                                        ��                  �  �                  ��W	                       �  ��  ��  ��     (}                                        ��                  �                    ��W	                       �  �  �  �     <}                                        ��                    "                  P�W	                         ��  ��  ��     P}                                        ��                  #  ?                  �8X	                       #  ,�  4�  $�     d}                                        ��                  @  \                  09X	                       @  ��  ��  ��     x}                                        ��                  ]  y                   :X	                       ]  D�  L�  <�     �}                                        ��                  z  �                  �:X	                       z  ��      ��     �}                                        ��                  �  �                  �;X	                       �  \�      O   �  ��  ��  �}               \�          D�  P�   , $�                                                       �     ��                            ����                            <�  d�  X�  ��      ��     Z     d�                      � `�  '                     ��    �  �  ��      �}      4   �����}                ��                      ��                  �  �                  ��W	                       �  ,�  �  /   �  ��     ��                          3   �����}            �                      3   �����}  ��  /   �  @�     P�                          3   ����~            p�                      3   ����(~  ��  /   �  ��     ��                          3   ����D~            ��                      3   ����d~      /   �  �     (�                          3   �����~            H�                      3   �����~  �~     
                @                     ��  @        
 P�              � ߱        ��  V   0  X�  ���                        ��  $  J  �  ���                       ��                         � ߱        Ā     
                @�                     ��  @        
 P�              � ߱        ��  V   T  @�  ���                        ��  $  n  ��  ���                       ��     
                    � ߱        ��     
                ,�                     |�  @        
 <�              � ߱        ��  V   x  (�  ���                        t�  $  �  ��  ���                       ��     
                    � ߱        ��     
                �                     h�  @        
 (�              � ߱        ��  V   �  �  ���                        \�  $  �  ��  ���                       ��                         � ߱        ��     
                $�                     t�  @        
 4�              � ߱        ��  V   �  ��  ���                        ��  �   �  ��      X�  $  �  ��  ���                       ��     
                    � ߱        ��     
                <�                     ��  @        
 L�              � ߱        ��  V   �  ��  ���                        ��  $     ��  ���                       ��     
                    � ߱        ��  �     ��      H�  $  $  �  ���                       �     
                    � ߱        \�  �   >   �      ��  $  `  ��  ���                       @�                         � ߱              k  ��  ��      \�      4   ����\�      /   l  �     �                          3   ����|�  L�     
   <�                      3   ������  |�        l�                      3   ������  ��        ��                      3   ������            ��                      3   ����ԋ  pushRowObjUpdTable  ��  ��  �                   [      �                               d                     pushTableAndValidate    ��  L�  �           |     \     �                          �  �                     remoteCommit    d�  ��  �           p     ]     �                          �  �                     serverCommit    ��  ,�  �           l     ^     �                          �  �                                     L�          �  �      ��                  �  �  4�              �Y	                    O   ����    e�          O   ����    R�          O   ����    ��          O   �  ��  ��  �    ��                            ����                            <�  P�      ��              _      d�                      
�     �                     disable_UI  ��   �                      `      �                               �  
                    �  �    ����  �       ��          ��  8   ����   ��  8   ����   ��  8   ����   ��  8   ����       8   ����       8   ����        �  �      viewObject  ,   ��   �  ,�      toggleData  ,INPUT plEnabled LOGICAL    �  X�  p�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  H�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  8�  D�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE (�  ��  ��      removeAllLinks  ,   ��  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  $�  8�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    �  ��  ��      hideObject  ,   ��  ��  ��      exitObject  ,   ��  ��  �      editInstanceProperties  ,   ��  �  ,�      displayLinks    ,   �  @�  P�      createControls  ,   0�  d�  t�      changeCursor    ,INPUT pcCursor CHARACTER   T�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  @�  L�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER 0�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��   �  �      unbindServer    ,INPUT pcMode CHARACTER ��  8�  H�      runServerObject ,INPUT phAppService HANDLE  (�  t�  ��      disconnectObject    ,   d�  ��  ��      destroyObject   ,   ��  ��  ��      bindServer  ,   ��  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  4�  @�      startFilter ,   $�  T�  d�      releaseDBRow    ,   D�  x�  ��      refetchDBRow    ,INPUT phRowObjUpd HANDLE   h�  ��  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE ��  ��  �      fetchDBRowForUpdate ,   ��  $�  4�      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL �  d�  t�      compareDBRow    ,   T�  ��  ��      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   x�  �  �      assignDBRow ,INPUT phRowObjUpd HANDLE    �  H�  T�      updateState ,INPUT pcState CHARACTER    8�  ��  ��      updateQueryPosition ,   p�  ��  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    ��   �  �      undoTransaction ,   ��  $�  4�      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  �  ��  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   ��  ,�  @�      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   �  ��  ��      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  |�  �  $�      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  �  h�  |�      startServerObject   ,   X�  ��  ��      setPropertyList ,INPUT pcProperties CHARACTER   ��  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��   �  0�      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    �  ��   �      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  ,�  <�      rowObjectState  ,INPUT pcState CHARACTER    �  h�  x�      retrieveFilter  ,   X�  ��  ��      restartServerObject ,   |�  ��  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   ��  ��  ��      refreshRow  ,   ��  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  P�  `�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  @�  ��  ��      initializeServerObject  ,   ��  ��  ��      initializeObject    ,   ��  ��  ��      home    ,   ��   �  �      genContextList  ,OUTPUT pcContext CHARACTER ��  <�  H�      fetchPrev   ,   ,�  \�  h�      fetchNext   ,   L�  |�  ��      fetchLast   ,   l�  ��  ��      fetchFirst  ,   ��  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   ��  ��  �      endClientDataRequest    ,   ��   �  4�      destroyServerObject ,   �  H�  X�      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    8�  ��  ��      dataAvailable   ,INPUT pcRelative CHARACTER ��  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  4�  H�      commitTransaction   ,   $�  \�  l�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    L�  �  �      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 f	%     adecomm/as-utils.w 
"   
   �    }        �
"     
    �     }        �� ,   :   %               � 
" 	   
 �%              h �P  \         (          
�                          
�            � w   L	
" 	   
 g	
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 �               1� �  
 � �   �%               o%   o           � �    
"   
 �           �    1� �   � �   �%               o%   o           � �   
"   
 �           �    1� �  
 � �   �%               o%   o           � �   
"   
 �           l    1� �   � �   �%               o%   o           � �    
"   
 �           �    1� �   � �   �%               o%   o           � �   
"   
 �           T    1� �   �    �%               o%   o           %               
"   
 ��          �    1�    ��      
"   
 �               1� %   � �   �%               o%   o           � 8  
"   
 �           �    1� :   � �   �%               o%   o           � I  S 
"   
 �           �    1� �   �    �%               o%   o           %               
"   
 �           p    1� �   �    �%               o%   o           %               
"   
 �           �    1� �   �    �%               o%   o           %              
"   
 ��          h    1� �   ��      
"   
 �           �    1� �  
 �    �%               o%   o           %               
"   
 �                1� �   � �   �%               o%   o           � �    
"   
 ��          �    1� �   ��      
"   
 �           �    1� �   � �   �%               o%   o           �   t 
"   
 ��          D	    1� �  
 ��      
"   
 �           �	    1� �   � �   �%               o%   o           � �  � 
"   
 �           �	    1� 2   � �   �%               o%   o           � �    
"   
 �           h
    1� I  
 � T   �%               o%   o           %               
"   
 g	�           �
    1� X   g	�    �%               o%   o           %              
"   
 g	�           `    1� `   g	� �   �%               o%   o           � �    g	
"   
 g	�           �    1� q   g	� �   �%               o%   o           o%   o           
"   
 g	�           P    1� �  
 g	� �   �%               o%   o           � �    f	
"   
 g	�           �    1� �   g	� �  	 �%               o%   o           � �  / g	
"   
 ��          8    1� �   �� �  	   
"   
 f	�           t    1� �   f	� �  	 �o%   o           o%   o           � �    f	
"   
 ��          �    1� �   �� �  	   
"   
 g	�           $    1�    g	� �  	 �o%   o           o%   o           � �    g	
"   
 ��          �    1�    ��      
"   
 ��          �    1� )   �� �  	   
"   
 ��              1� 6   �� �  	   
"   
 ��          L    1� C   �� �  	   
"   
 g	�           �    1� Q   g	�    �o%   o           o%   o           %              
"   
 ��              1� b   �� �  	   
"   
 ��          @    1� p  
 �� {     
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
 g	�                1� �   g	� �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 h	
"   
   
"   
 L	(�  L ( l       �        �    �� 	   � P   �        �    �@    
� @  , 
�            ��      p�               �L
�    %              � 8          � $         �           
�    � 3     
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 g	�           �    1� 6  
 g	� �   �%               o%   o           � �    g	
"   
 g	�           <    1� A  
 g	� �   �%               o%   o           o%   o           
"   
 g	�           �    1� L   g	�    �%               o%   o           o%   o           
"   
 g	�           4    1� U   g	�    �%               o%   o           %               
"   
 g	�           �    1� d   g	�    �%               o%   o           %               
"   
 f	�           ,    1� q   f	� �   �%               o%   o           � �    g	
"   
 g	�           �    1� x   g	�    �%               o%   o           %              
"   
 g	�               1� �   g	�    �%               o%   o           o%   o           
"   
 g	�           �    1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�               1� �  	 g	� �   �%               o%   o           � �    f	
"   
 g	�           �    1� �   g	� �   �%               o%   o           o%   o           
"   
 h	�               1� �   h	� �   �%               o%   o           o%   o           
"   
 g	�           �    1� �   g	�    �%               o%   o           %               
"   
 g	�           �    1� �   g	�    �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 h	�           �    1� �  
 h	�    �%               o%   o           %              
"   
 h	�           H    1� �   h	� �   �%               o%   o           o%   o           
"   
 g	�           �    1�    g	� �   �%               o%   o           � �    g	
"   
 g	�           8    1�    g	� �   �%               o%   o           o%   o           
"   
 ��          �    1�    ��      
"   
 g	�           �    1� +   g	� �   �%               o%   o           � >  ! h	
"   
 g	�           d    1� `   g	� �   �%               o%   o           � �    g	
"   
 f	�           �    1� m   f	� �   �%               o%   o           � �   g	
"   
 ��          L    1� �   �� �     
"   
 ��          �    1� �   ��      
"   
 g	�           �    1� �   g	� �   �%               o%   o           � �    g	
"   
 ��          8     1� �  
 ��      
"   
 f	�           t     1� �   f	�    �%               o%   o           o%   o           
"   
 g	�           �     1� �   g	�    �%               o%   o           %               
"   
 g	�           l!    1� �   g	�    �%               o%   o           %               
"   
 g	�           �!    1� �   g	� �   �%               o%   o           � �    g	
"   
 g	�           \"    1� 	   g	� �   �%               o%   o           o%   o           
"   
 g	�           �"    1�    g	�    �%               o%   o           %              
"   
 g	�           T#    1� &   g	�    �%               o%   o           %               
"   
 f	�           �#    1� 3   f	�    �%               o%   o           %               
"   
 ��          L$    1� C   ��      
"   
 ��          �$    1� P   �� �     
"   
 g	�           �$    1� ]   g	� T   �%               o%   o           o%   o           
"   
 g	�           @%    1� i   g	� �   �%               o%   o           � �    f	
"   
 g	�           �%    1� w   g	� �   �%               o%   o           o%   o           
"   
 h	�           0&    1� �   h	�    �o%   o           o%   o           o%   o           
"   
 h	�           �&    1� �   h	� �  	 �%               o%   o           o%   o           
"   
 g	�           ('    1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�           �'    1� �  
 g	� T   �%               o%   o           o%   o           
"   
 ��           (    1� �   �� �     
"   
 g	�           \(    1� �   g	� �   �%               o%   o           � �  4 g	
"   
 g	�           �(    1�    
 g	�    �%               o%   o           %              
"   
 ��          L)    1� +   ��      
"   
 h	�           �)    1� <   h	� �   �%               o%   o           � �    f	
"   
 f	�           �)    1� J   f	�    �%               o%   o           %              
"   
 g	�           x*    1� Y   g	� �   �%               o%   o           � �    f	
"   
 g	�           �*    1� f   g	� �   �%               o%   o           � �    g	
"   
 g	�           `+    1� t   g	� �   �%               o%   o           � �    g	
"   
 h	�           �+    1� �   h	�    �%               o%   o           %               
"   
 h	�           P,    1� �  	 h	�    �%               o%   o           o%   o           
"   
 f	�           �,    1� �   f	� �   �%               o%   o           � �  	 g	
"   
 h	�           @-    1� �   h	� T   �%               o%   o           %       �       
"   
 f	�           �-    1� �   f	� �   �%               o%   o           � �    h	
"   
 f	�           0.    1� �   f	�    �o%   o           o%   o           %              
"   
 g	�           �.    1� �   g	�    �%               o%   o           %               
"   
 g	�           (/    1� �   g	� �   �%               o%   o           o%   o           
"   
 h	�           �/    1� �   h	� �  	 �%               o%   o           � �    g	
"   
 ��          0    1�    �� �  	   P �L 
�H T   %              �     }        �GG %              
"   
 f	�           �0    1�   
 f	� �   �%               o%   o           � �    f	
"   
 g	�           1    1� (   g	�    �%               o%   o           %               
"   
 f	�           �1    1� 5  	 f	� �   �%               o%   o           � �    g	
"   
 g	�           2    1� ?   g	� �   �%               o%   o           � �    f	
"   
 g	�           �2    1� M   g	�    �%               o%   o           %               
"   
 g	�           �2    1� ]   g	� �   �%               o%   o           � �    g	
"   
 g	�           p3    1� p   g	� �   �%               o%   o           o%   o           
"   
 g	�           �3    1� x   g	� �   �%               o%   o           o%   o           
"   
 g	�           h4    1� �   g	�    �%               o%   o           o%   o           
"   
 f	�           �4    1� �   f	�    �%               o%   o           o%   o           
"   
 g	�           `5    1� �   g	�    �%               o%   o           o%   o           
"   
 f	�           �5    1� �   f	� �   �%               o%   o           o%   o           
"   
 h	�           X6    1� �  	 h	� �  	 �%               o%   o           � �    g	
"   
 g	�           �6    1� �  
 g	� �  	 �%               o%   o           � �    h	
"   
 h	�           @7    1� �   h	� �   �%               o%   o           � �    g	
"   
 h	�           �7    1� �   h	� �   �%               o%   o           o%   o           
"   
 g	�           08    1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�           �8    1�    g	� �   �%               o%   o           � �    g	
"   
 f	�            9    1�    f	� �   �%               o%   o           � �    g	
"   
 f	�           �9    1� &   f	� �  	 �%               o%   o           o%   o           
"   
 ��          :    1� 8   ��      
"   
 g	�           L:    1� D   g	� �   �%               o%   o           � �    h	
"   
 g	�           �:    1� R   g	� �   �%               o%   o           o%   o           
"   
 f	�           <;    1� e   f	�    �%               o%   o           o%   o           
"   
 g	�           �;    1� w  
 g	� �   �%               o%   o           � �    g	
"   
 g	�           ,<    1� �   g	� �   �%               o%   o           � �    g	
"   
 g	�           �<    1� �   g	�    �%               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 g	�           p=    1� �  	 g	�    �%               o%   o           o%   o           
"   
 g	�           �=    1� �   g	�    �%               o%   o           o%   o           
"   
 h	�           h>    1� �   h	�    �%               o%   o           o%   o           
"   
 f	�           �>    1� �   f	�    �%               o%   o           %              
"   
 g	�           `?    1� �   g	� �   �%               o%   o           �    M f	
"   
 g	�           �?    1� N   g	�    �%               o%   o           %              
"   
 g	�           P@    1� _   g	�    �%               o%   o           %               
"   
 f	�           �@    1� s   f	�    �%               o%   o           %               
"   
 h	�           HA    1� �   h	� �  	 �%               o%   o           � �   f	
"   
 h	�           �A    1� �   h	�    �%               o%   o           %               
"   
 h	�           8B    1� �   h	� �  	 �%               o%   o           o%   o           
"   
 g	�           �B    1� �   g	�    �o%   o           o%   o           %              
"   
 f	�           0C    1� �   f	� �  	 �o%   o           o%   o           � �    f	
"   
 g	�           �C    1� �   g	�    �o%   o           o%   o           o%   o           
"   
 g	�            D    1� �   g	�    �o%   o           o%   o           o%   o           
"   
 g	�           �D    1� 	   g	� �  	 �o%   o           o%   o           o%   o           
"   
 g	�           E    1�    g	�    �o%   o           o%   o           o%   o           
"   
 g	�           �E    1� (   g	� �  	 �o%   o           o%   o           � 6   g	
"   
 g	�           F    1� 8   g	� �  	 �o%   o           o%   o           � G   g	
"   
 f	�           |F    1� S   f	�    �%               o%   o           %               
"   
 g	�           �F    1� g   g	�    �%               o%   o           %               
"   
 ��          tG    1� {   �� �  	   
"   
 g	�           �G    1� �   g	�    �%               o%   o           %               
"   
 g	�           ,H    1� �   g	� �   �%               o%   o           o%   o           
"   
 h	�           �H    1� �   h	� �   �%               o%   o           o%   o           
"   
 h	�           $I    1� �   h	�    �%               o%   o           o%   o           
"   
 g	�           �I    1� �   g	� �   �%               o%   o           � �    g	
"   
 g	�           J    1� �   g	� �   �%               o%   o           %               
"   
 g	�           �J    1� �  	 g	�    �%               o%   o           %                "    �%     start-super-proc v�%     adm2/smart.p -L	P �L 
�H T   %              �     }        �GG %              
"   
   �       �K    6� 	     
"   
   
�        �K    8
"   
   �        �K    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        �M    �� 	   � P   �        �M    �@    
� @  , 
�       �M    ��    L	p�               �L
�    %              � 8       N    � $         �           
�    � 3   L	
"   
 �p� @  , 
�       O    �� %   �p�               �L"  	  , �   � 0   g	� 2   ��     }        �A      |    "  	    � 0   g	%              (<   \ (    |    �     }        �A� 4   �A"  
  g	    "  	  L	"  
  g	  < "  	  L	"  
  g	(    |    �     }        �A� 4   �A"  
  g	
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        �P    �� 	   � P   �        �P    �@    
� @  , 
�       �P    ��    L	p�               �L
�    %              � 8      Q    � $         �           
�    � 3   L	
"   
 �p� @  , 
�       R    �� �  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 g	
"   
   
"   
   (�  L ( l       �        �R    �� 	   � P   �        �R    �@    
� @  , 
�       �R    ��      p�               �L
�    %              � 8      �R    � $         �           
�    � 3     
"   
 �p� @  , 
�       �S    �� �  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�       XT    �� �     p�               �L%               
"   
  p� @  , 
�       �T    ��     p�               �L%               
"   
  p� @  , 
�       U    �� �    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 h	 (   � 
"   
 L	    �        �U    �� 	   �
"   
   � 8      DV    � $         �           
�    � 3   L	
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6� 	     
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    � ]   g	
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 L	    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 h	"      �       }        �
"   
 �%              %                "    �%     start-super-proc u�%     adm2/appserver.p �g	�    � �     
�    �     }        �%               %      Server  - �     }        �    "    g	� �    �%                   "    g	� �    �%      NONE    p�,  8         $     "    f	        � �   L	
�    
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        �Z    �� 	   � P   �        �Z    �@    
� @  , 
�       �Z    ��    L	p�               �L
�    %              � 8      �Z    � $         �           
�    � 3   L	
"   
 �p� @  , 
�       �[    �� �   �p�               �L"    , p�,  8         $     "    f	        �    L	
�     "    �%     start-super-proc t�%     adm2/dataquery.p �f	
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
 L	(�  L ( l       �        ]    �� 	   � P   �        ]    �@    
� @  , 
�       $]    ��    L	p�               �L
�    %              � 8      0]    � $         �    L	     
�    � 3   L	
"   
 �p� @  , 
�       @^    �� �   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
 L	(�  L ( l       �        $_    �� 	   � P   �        0_    �@    
� @  , 
�       <_    ��    L	p�               �L
�    %              � 8      H_    � $         �    L	     
�    � 3   L	
"   
 �p� @  , 
�       X`    �� �   �p�               �L%               "    �%     start-super-proc t�%     adm2/query.p -L	%     start-super-proc t�%     adm2/queryext.p % 	    initProps L	
�    %4 + $   FOR EACH gn-clie NO-LOCK INDEXED-REPOSITION �   � s     � u     �       
�     	         �G
"   
 h	�        �a    �G
"   
   
"   
    x    (0 4      �        �a    �G%                   �        b    �GG %              � Y    L	� Z         %              %                   "      %              
"   
       "      �        �b    �
"   
   �        (c    �
"   
   
�       Hc    �"       \      H   "    L	((       "      %              � �      � s   L	     
"   
   
"   
 � \      H   "      ((       "      %              � �     � s   g	�        �c    �%                   %              %                   "  (    %                  "  (        
"   
 L	
"   
 g	0 T       m � "  (  f	�        �d    �A @   "      $         � "  (  g	� 4   ��        e    �� "  (    
"   
 � \ H     H   "      ((       "    L	%              � �    �� s     (        "  !  L	� �    g	�        �e    �"  !  �
�H T   %              �     }        �GG %              
"   
 h	
"   
 h	
"   
   
"   
   (�  L ( l       �        �f    �� 	   � P   �        �f    �@    
� @  , 
�       �f    ��      p�               �L
�    %              � 8      �f    � $         �           
�    � 3     
"   
 �p� @  , 
�       �g    �� �   �p�               �L%               
"   
   p� @  , 
�       Dh    ��      p�               �L"    , �,  8         $     "    �L        � a  
  
�    
�H T   %              �     }        �GG %              
"   
 $ 
"   
   
"   
 L	
"   
 L	(�  L ( l       �        (i    �� 	   � P   �        4i    �@    
� @  , 
�       @i    ��    L	p�               �L
�    %              � 8      Li    � $         �    L	     
�    � 3     
"   
 �p� @  , 
�       \j    �� 8   �p�               �L
"   
 , 
"   
   p� @  , 
�       �j    ��      p�               �L"    , 
"   
  p� @  , 
�       k    �� �    p�               �L"    ,     "    h	� �    �%L B <   OPEN QUERY Query-Main FOR EACH gn-clie NO-LOCK INDEXED-REPOSITION.     "    cl� �    I((        "    OS%                   "    �� �     "    L	 (   "           "    �%              @ �,  8         $     "    L	        � �    
�    p�,  8         $     � �   g	        � �   L	
�    %               �    "      � u         %              %                   "      %                  "      "      "     T(        "    g	%              "    g	� u   �"      �       "    L	�    "    g	� 4   �� �      � 4   L	�    "     � 4    S    "      "    �    "    g	%                � @    �     t T     P   4       �"      (0       4       g	"      � �      � �    L	� s   g	T ,  %              T   "    g	"    �� u     � 4   L	� s   g	T    �    "    g	� 4   �"      � 4   L	"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "    L	%              � �    �� �     4  g	     "      
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        �q    �� 	   � P   �        �q    �@    
� @  , 
�       �q    ��    L	p�               �L
�    %              � 8      �q    � $         �           
�    � 3   L	
"   
 �p� @  , 
�       �r    ��   
 �p�               �L"    ,       "  
  g	�    � �  ! g	� u   �      "  	    �    � �  ! �� u   g	�   � s     � u     � �  ! L	�   � s     � u   L	� �  ! g	�   � s     � u     � �  !   
�H T   %              �     }        �GG %              
"   
 �
"   
 L	
"   
 �
"   
 �(�  L ( l       �        Dt    �� 	   � P   �        Pt    �@    
� @  , 
�       \t    ��    �p�               �L
�    %              � 8      ht    � $         �           
�    � 3     
"   
 �p� @  , 
�       xu    �� �   �p�               �L"    , 
"   
   p� @  , 
�       �u    �� �     p�               �L"    , 
"   
  p� @  , 
�       (v    �� ]    p�               �L"    ,     %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    � �  !   � u         "  	  g	�     "    g	T    "      "      @ A,    �   � s   �� �     "    L	"       T      @   "    �(        "      � �    L	� �      � s   L	"    f	     "  	   %              D H   @ A,    �   � s   L	� �     "    L	"    h	,    S   "    L	� �  ! h	� u   �%                T      @   "    �(        "      � �    L	� �      � s   L	"    h	     "  
   %                         "    �� �     "    L	           "      � �   L	"      
�H T   %              �     }        �GG %              
"   
 h	
"   
   
"   
 h	
"   
 L	(�  L ( l       �        Dz    �� 	   � P   �        Pz    �@    
� @  , 
�       \z    ��    h	p�               �L
�    %              � 8      hz    � $         �    L	     
�    � 3   �
"   
 �p� @  , 
�       x{    �� �   �p�               �L"    , 
"   
   p� @  , 
�       �{    �� ]     p�               �L"    , "      %               �     }        �%              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    �%     start-super-proc s�%     adm2/data.p %     start-super-proc s�%     adm2/dataext.p 	%     start-super-proc s�%     adm2/dataextcols.p 	%     start-super-proc s�%     adm2/dataextapi.p f	
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
 L	(�  L ( l       �            �� 	   � P   �            �@    
� @  , 
�       (    ��    L	p�               �L
�    %              � 8      4    � $         �    L	     
�    � 3   L	
"   
 �p� @  , 
�       D�    �� �   �p�               �L%               %     "Util/dgn-clie.i"   
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        �    �� 	   � P   �        �    �@    
� @  , 
�       (�    ��    L	p�               �L
�    %              � 8      4�    � $         �           
�    � 3   L	
"   
 �p� @  , 
�       D�    �� �   �p�               �L"    , 
�     	        �G
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        ��    �� 	   � P   �        �    �@    
� @  , 
�       �    ��    L	p�               �L
�    %              � 8       �    � $         �           
�    � 3   L	
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
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        �    �� 	   � P   �        �    �@    
� @  , 
�        �    ��    L	p�               �L
�    %              � 8      �    � $         �           
�    � 3   L	
"   
 �p� @  , 
�       �    �� �  	 �p�               �L
"   
 , 
"   
 �     �   	   �        t�    �
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        �    �� 	   � P   �         �    �@    
� @  , 
�       �    ��    L	p�               �L
�    %              � 8      �    � $         �           
�    � 3   L	
"   
 �p� @  , 
�       (�    �� �   �p�               �L"    , 
"   
   �       ��    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        �    �� 	   � P   �        �    �@    
� @  , 
�       $�    ��    L	p�               �L
�    %              � 8      0�    � $         �           
�    � 3   L	
"   
 �p� @  , 
�       @�    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 h	        �    L	
�    
�             �Gp�,  8         $     
"   
 h	        � )   L	
�    �    � ;     
�        "    h	� �    �%     modifyListProperty 
�    %      REMOVE  %     SupportedLinks 	%     Update-Target  	%     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � �     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           �   l       ��                 Y  }  �               ��e	                    O   ����    e�          O   ����    R�          O   ����    ��        $  h  �   ���                       �U     
                    � ߱              i  (  �      V      4   ����V                �                      ��                  j  |                  �zg	                       j  8  �  �  k  PV            m  �  `      �V      4   �����V                p                      ��                  n  {                  ({g	                       n  �  �  o   o      ,                                 �  �   p  �V      �  �   q  �V      $  $  r  �  ���                        W     
                    � ߱        8  �   s  @W      L  �   t  `W      `  �   w  �W          $   z  �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �                0g	                    O   ����    e�          O   ����    R�          O   ����    ��      ~                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  �Mg	                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �   �  �X          O   �  ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��            	     H  �  �               ܤg	                    O   ����    e�          O   ����    R�          O   ����    ��        $  h  �   ���                       `a                         � ߱        �  $  i  8  ���                       �a                         � ߱        �a     
                b  @         �a              � ߱        D  $   �  d  ���                         T      �  x                      ��        0         �  �                  ��g	      4c     L     �  �      $  �  �  ���                       �b                         � ߱          $  �  �  ���                       �b                         � ߱            4   ���� c  Tc     
                tc                     �c                         � ߱          $  �    ���                                                            ��                  �  �                  t�g	                �     �  �  �  $  �  L  ���                       pd       !       !           � ߱          �      L  �                      ��        0         �  �                  �qh	     ( �d            �  x      $  �     ���                       �d       (       (           � ߱        �  $  �  x  ���                       �d       (       (           � ߱            4   �����d        �  �  L      e      4   ����e                \                      ��                  �  �                  �th	                       �  �  �  $  �  �  ���                       �e       !       !           � ߱            O   �  �� ��          $  �  �  ���                       �e                         � ߱        df     
                �f                     0h  @        
 �g          �h  @        
 Ph          �h                     �h     
                Xi                     �j  @        
 hj           k  @        
 �j          Xk  @        
 k              � ߱        x  V   �  $  ���                        P	    |  �  $	      dk      4   ����dk  �k                     �k                     �k                     Pl                         � ߱            $  }  �  ���                       �	    �  l	  |	      �l      4   �����l      �   �  �l      �	  $  �  �	  ���                       m                         � ߱        �
  $  �  
  ���                       $m                         � ߱          �
                              ��        0         �  �                  �wh	      �m     �     �  @
      $  �  �
  ���                       Dm                         � ߱        l  $  �  @  ���                       tm                         � ߱            4   �����m  �m                     n                     n                     ln                     �n                         � ߱        D  $  �  |  ���                             �  `  p      �n      4   �����n      $  �  �  ���                       �n           p             � ߱        �  $  �  �  ���                       p                         � ߱          �      �  \                      ��        0         �  �                  �xh	      �p          �         $  �  �  ���                        p                         � ߱        L  $  �     ���                       Pp                         � ߱            4   ����xp      $  �  �  ���                       �p                         � ߱        4q     
                �q                      s  @        
 �r              � ߱        �  V   �  �  ���                        s       
       
       @s       	       	       ts                     �s                         � ߱        �  $    D  ���                       �  $  �    ���                       �s                         � ߱        �s     
                tt                     �u  @        
 �u          v  @        
 �u          tv  @        
 4v              � ߱        |  V   �  H  ���                          �      �  \                      ��        0    	     &  ;                  �6Y	       w     4     &        $  &  �  ���                       �v                         � ߱        <  $  &    ���                       �v                         � ߱        L  4   �����v      4   ����w  �  $  +  �  ���                       xw                         � ߱        �    -  �  L      �w      4   �����w                �                      ��                  .  2                  7Y	                       .  �  �w                     Dx       	       	           � ߱            $  /  \  ���                             4  �  h      lx      4   ����lx  	              �                      ��             	     6  :                  �7Y	                       6  �   y                     hy       
       
           � ߱            $  7  x  ���                       �y                     �y                         � ߱        �  $  A  �  ���                       �y     
                tz                     �{  @        
 �{          |  @        
 �{              � ߱            V   O  `  ���                                    J `          �  �  � X@                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
             
                                                                                                                                                                                                                               ) �   �   �   �       (  8  H  X  h  x  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX     ) �   �   �   �      (  8  H  X  h  x  �  �  �  �  �   �     (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX  �   :                  � �                     �    ��                      ��                            ����                                                       `h                                �   l       ��                  �  	  �               �U	                    O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   l       ��                    "  �               ��U	                    O   ����    e�          O   ����    R�          O   ����    ��      w       �              �                  $                  d  /    $     4  �                      3   ������            T                      3   �����      O      ��  ��   �               �          �  �    �                                             ��                            ����                                            H          �   l       ��                  ,  W  �               p&X	                    O   ����    e�          O   ����    R�          O   ����    ��      �       �              �                $                  �       ,             �          �                                �  /  K  t     �  H�                      3   ����$�            �                      3   ����P�     /  M  �     �  x�                      3   ����\�  x                             3   ������      $   M  L  ���                                                   � ߱                  �  �                  3   ������      $   M  �  ���                                                   � ߱        X  $  Q  ,  ���                       ��                         � ߱            O   U  ��  ��  ��               �          �  �   @ �                                                              0              0           ��                            ����                                                       �   l       ��                  a  �  �               PX	                    O   ����    e�          O   ����    R�          O   ����    ��      �       $                  �                    �          �                      �              /    L     \  �                      3   ����Ȍ  �        |  �                  3   �����      $     �  ���                                                   � ߱                                      3   ������      $     @  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   l       ��                  �  �  �               �f	                    O   ����    e�          O   ����    R�          O   ����    ��            �  �   �       �      4   �����      �   �  ,�    ��                            ����                            TXS appSrvUtils gn-clie Maestro de Clientes O:\on_in_co\Util\dgn-clie.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "Util/dgn-clie.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH gn-clie NO-LOCK INDEXED-REPOSITION ,   hQuery cOpenQuery cDBNames cPhysicalTables cKeyTableEntityFields cKeyTableEntityValues cKeyTableEntityMnemonic cKeyTableEntityObjField cDBName cEntityFields lHasObjectField lHasAudit lHasComment lHasAutoComment iLookup iAlias  STATIC setDBNames OPEN QUERY Query-Main FOR EACH gn-clie NO-LOCK INDEXED-REPOSITION.  FOR   PRESELECT  setOpenQuery 5 showMessage ; NomCli DirCli NroCard Ruc CodDept Query-Main INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p NomCli DirCli NroCard Ruc CodDept RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI qDataQuery   40  @  l>      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   K	  c	  e	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props h  i  j  k  m  n  o  p  q  r  s  t  w  z  {  |  }            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �  �  �           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable   �        |     cQueryString    �        �  
   hQuery  �        �  
   hBuffer �        �     cOpenQuery          �     cDBNames    (             cPhysicalTables T        <     cKeyTableEntityFields   �        h     cKeyTableEntityValues   �        �     cKeyTableEntityMnemonic �         �     cKeyTableEntityObjField �     !   �     cDBName      "        cEntityFields   <     #   ,     lHasObjectField \     $   P     lHasAudit   |     %   p     lHasComment �     &   �     lHasAutoComment �     '   �     iLookup        (   �     iAlias  |    3   Y   �                            initProps   h  i  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  |  }  �  �  �  �  �  �  �  �  �  �  �  �  �  �    �  �  &  +  -  .  /  2  4  6  7  :  ;  A  O  �            �     lRet                      piTableIndex    �  h  *   Z   �  �      T                  deleteRecordStatic  �  �  �  �  �  �  �  �           :  ;  W  X  t  u  �  �  �  �  �  �  �  �      "  #  ?  @  \  ]  y  z  �  �  �  �  �  �                 !       $  l     [             X                  pushRowObjUpdTable  	  �        �        pcValType                  $       (  �     \       p      �                  pushTableAndValidate         "  $                pcContext   <             $       `        T        pcMessages            x        pcUndoIds   �  �     ]              �                  remoteCommit    K  M  Q  U  W  �             $                       pcMessages            ,        pcUndoIds   �  x     ^       �      h                  serverCommit      �  8  �     _               �                  getRowObjUpdStatic  �  �  �       `               �                  disable_UI  �  �  �  �!  
     4      <!                      ,  P  \  
   RowObject   �         �         �         �         �         �                                               NomCli  DirCli  NroCard Ruc CodDept RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     <  H     RowObjUpd   �         �         �         �         �         �         �                                    $         NomCli  DirCli  NroCard Ruc CodDept RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   T          H  
   appSrvUtils |       h     xiRocketIndexLimit  �        �  
   gshAstraAppserver   �        �  
   gshSessionManager   �        �  
   gshRIManager              
   gshSecurityManager  @        ,  
   gshProfileManager   l  	 	     T  
   gshRepositoryManager    �  
 
     �  
   gshTranslationManager   �        �  
   gshWebManager   �        �     gscSessionId            �     gsdSessionObj   (          
   gshFinManager   L        <  
   gshGenManager   p        `  
   gshAgnManager   �        �     gsdTempUniqueID �        �     gsdUserObj  �        �     gsdRenderTypeObj            �     gsdSessionScopeObj            
   ghProp  @       4  
   ghADMProps  d       T  
   ghADMPropsBuf   �       x     glADMLoadFromRepos  �       �     glADMOk �       �  
   ghContainer �    	   �     cObjectName      
   �     iStart  $              cAppService D        8      cASDivision p        X      cServerOperatingMode    �        �      cContainerType  �        �      cQueryString    �        �   
   hRowObject  �        �   
   hDataQuery  !       !     cColumns             ,!     cDataFieldDefs  T!       L!  gn-clie p!    X  d!  RowObject         X  �!  RowObjUpd            9   �   �   �   �           3  ?  @  A  C  E  F  G  K  L  O  P  Q  R  T  V  X  Z  [  \  _  a  b  d  e  f  g  h  n  p  v  x  z  {  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  <
  =
  ?
  @
  A
  B
  C
  D
  F
  G
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
  p
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
  �
  �
                     y  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  (  �  �  �  �  �  �  �  �  �  �    +  J  L  a  �        ,  -  .  1  2  3  :  ;  X  l  �      +  O  �  �  �  �  �  <  �  �  �  �  �  �  �  0  J  T  n  x  �  �  �  �  �  �  �       $  >  `  k  l      ��  C:\Progress\OpenEdge\src\adm2\data.i �%  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    �%  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i $&  lo , O:\on_in_co\Util\dgn-clie.i  X&  �:  C:\Progress\OpenEdge\src\adm2\query.i    �&  z + C:\Progress\OpenEdge\src\adm2\delrecst.i �&  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  �&  F� ) C:\Progress\OpenEdge\gui\fnarg   '   ( %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   H'  �   C:\Progress\OpenEdge\src\adm2\dataquery.i    �'  �Z ' %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   �'  �< ! C:\Progress\OpenEdge\src\adm2\appserver.i    (  �� & %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   <(  I� " C:\Progress\OpenEdge\src\adm2\smart.i    �(  Ds % C:\Progress\OpenEdge\gui\fn  �(  tw $ %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �(  Q. # C:\Progress\OpenEdge\gui\set )  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i D)  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    x)  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    �)  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i   *  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i 4*  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i t*   	 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    �*  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   �*  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   8+  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �+  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �+  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �+  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i <,  �j  C:\Progress\OpenEdge\gui\get p,  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �,  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �,  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i  -  Su  C:\Progress\OpenEdge\src\adm2\globals.i  T-  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �-  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �-  �  C:\Progress\OpenEdge\src\adm2\appsprto.i .  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   @.  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    �.  ª 
 %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   �.  ��  C:\Progress\OpenEdge\src\adm2\qryprto.i  /  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   @/  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i �/  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    �/  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �/  ;�    O:\on_in_co\Util\dgn-clie.w      �   �      \0  [  T     l0     R  %   |0  �   �     �0     t  .   �0  �   j     �0     K     �0  �   H     �0     &  #   �0  �   $     �0       #   �0  �         1     �  #   1  �   �     ,1     �  #   <1  �   �     L1     �  #   \1  �   �     l1     p  #   |1  �   n     �1     L  #   �1  �   J     �1     (  #   �1  �        �1       -   �1  �   �     �1     �  ,   �1  k   �     2  �  �     2     �  +   ,2  �  �     <2     |  +   L2  �  y     \2     _  +   l2  �  \     |2     B  +   �2  �  ?     �2     %  +   �2  �  "     �2       +   �2  �       �2     �  +   �2  �  �     �2     �  +   3  �  �     3     �  +   ,3  �  �     <3     �  +   L3  �  �     \3     w  +   l3  �  t     |3     Z  +   �3  �  W     �3     =  +   �3  �  :     �3        +   �3  �       �3       +   �3  �        �3     �  +   4  �  �     4     �  +   ,4  �  �     <4     �  +   L4  �  �     \4     j  #   l4  �  i     |4     G  #   �4  k  "     �4        #   �4  j  �     �4     �  #   �4  i  �     �4     �  #   �4  _  �     �4     �  *   5  ^  �     5     c  *   ,5  ]  b     <5     <  *   L5  \  ;     \5       *   l5  [       |5     �  *   �5  Z  �     �5     �  *   �5  Y  �     �5     �  *   �5  X  �     �5     y  *   �5  W  x     �5     R  *   6  V  Q     6     +  *   ,6  U  *     <6       *   L6  T       \6     �  *   l6  S  �     |6     �  *   �6  R  �     �6     �  *   �6  Q  �     �6     h  *   �6  P  g     �6     A  *   �6  O  @     �6       *   7  N       7     �  *   ,7  @  �     <7     �  #   L7  	  �     \7     �  )   l7  �   y     |7     W  #   �7  �   V     �7     4  #   �7  �   3     �7       #   �7  �        �7     �  #   �7  �   �     �7     �  #   8  �   �     8     �  #   ,8  �   8     <8     �  (   L8  g   �     \8  a   �      l8     k  '   |8  _   i      �8     G  #   �8  ]   E      �8     #  #   �8  I         �8  �     !   �8     �  &   �8  �   �  !   �8     �  #   9  �   �  !   9     d  #   ,9  �   b  !   <9     @  #   L9  g   &  !   \9          l9  O   �  !   |9  �   y  "   �9     w  %   �9  �   G  "   �9     �  $   �9  �   �  "   �9     �  #   �9  �   �  "   �9     �  #   �9  �   �  "   :     |  #   :  �   {  "   ,:     Y  #   <:  �   E  "   L:     #  #   \:  }     "   l:     �  #   |:     y  "   �:     +  !   �:     �      �:     }     �:     1     �:  �   (     �:  O        �:     	     �:     �     ;  �   �     ;  �   y     ,;  O   k     <;     Z     L;          \;  y   �
     l;  �   �
  	   |;  G   �
     �;     �
     �;     t
     �;  c   
  	   �;  x   
     �;  M   �	     �;     �	     �;     �	     �;  a   �	     <  �  b	     <     C	     ,<  �  	     <<  O   	     L<     �     \<     �     l<  �   �     |<     �     �<     �     �<  x   �     �<     �     �<     ^     �<     Z     �<     F     �<     -     �<  Q        =     �     =     �     ,=     w     <=     ]     L=  ]   W  	   \=     M     l=       	   |=     �  
   �=     �  	   �=  Z   �     �=     �     �=     �     �=     �     �=     �     �=  c   a     �=     ?     >     �      >     �      ,>     �      <>     �      L>     !       \>           