	��V&8�K�6  ~ �                                              % 36A800EFutf-8 MAIN O:\on_in_co\Util\dccbcdocu.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,NroDoc character 0 0,RowNum integer 1 0,RowIdent character 2 0,RowMod character 3 0,RowIdentIdx character 4 0,RowUserProp character 5 0,ChangedFields character 6 0                      \             �   L�              8�              �=     +   �� �  W   4� `  X   ��   Y   ��   [   ��   \   п <  ]   �    ^   ,� 0  `   ? \� �  iSO8859-1                                                                           P    �                                      �                   �                �  �    �   ��   T�              ��  �   �      �          �                                             PROGRESS                         T	           
    
                    �              �                                                                                                     
               INTEGRAL                         PROGRESS                         �	        �                                }��K               *E                              �  l                      �  |  b_     CODCIACODDOCNRODOCFCHDOCCODCLINOMCLIDIRCLIRUCCLICODANTCODPEDNROPEDNROORDIMPBRTIMPEXOPORIGVIMPIGVIMPDTOIMPTOTSDOACTFLGESTCODCOBCODCTAUSUARIOFLGCIEFCHCIEHORCIECODMONTPOCMBCODALMLUGENTTIPOCODMOVCODVENIMPISCIMPVTAFCHCANGLOSACODREFNROREFFCHVTOCODAGEFLGUBIFLGUBIAFCHUBIFCHUBIAFLGSITCNDCRECODDIVIMPINTFMAPGOFCHACTFLGSITATIPVTAPORDTOTPOFACUSRDSCTOFLGCBDFCHCBDNROSALCODOPENROMESNROASTFCHANUUSUANUCODDPTOCODPROVCODDISTFLGCONLUGENT2FLGATEFCHATEIMPFLEIMPTOT2IMPCTOACUBONNROCARDTIPBONCCOFLGENVPUNTOSMRGUTISEDELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02FCHCRELIBRE_F03LIBRE_F04LIBRE_F05                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L 
        M          N 
        O          P          Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `           
  �      |	  
    
                  h	  0
             �	                                                                                          �          
  �
  �      (
  
    
                  
  �
             �
                                                                                          �          
  X        �
  
    
                  �
  �             D                                                                                                    
          �  
    
                  l  4             �                                                                                                    
  �  0      ,  
    
                    �             �                                                                                          0          
  \  B      �  
    
                  �  �  	           H                                                                                          B          
    W      �  
    
                  p  8  
           �                                                                                          W          
  �  m      0  
    
                    �             �                                                                                          m          
  `  {      �                         �  �             L                                                                                          {              �      �                        t  <             �                                                                                          �            �  �      4  
    
                     �             �                                                                                          �          
  d  �      �  
    
                  �  �             P                                                                                          �          
    �      �  
    
                  x  @             �                                                                                          �          
  �  �      8                        $  �             �                                                                                          �            h  �      �                        �  �             T                                                                                          �              �      �                        |  D                                                                                                        �                �      <                        (  �             �                                                                                          �            (         �       }  X  <     T  }  6�      �         }             �          <      �              �       �  X  �     �  �  �      4         �         �    �          h      �                 h�                                               l�            d  L l�                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                  p  x  �  �  �                         �  �  �  �                             �  �  �  �                              �  �  �  �                             �                                      $  ,  8                                                                          NroDoc  X(9)    Numero  Numero      RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  ���������           P        `        g                �     i     i     i     	 	 	    �  P  W  `  g  s                                                                                                                                                    �  �  �  �  �                         �  �                                         ,                              0  8  @  H                             L  X  `  l                             p  |  �  �                              �  �  �  �                                                                          NroDoc  X(9)    Numero  Numero      RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  ���������           P        `        g                �     i     i     i     	 	 	    �  P  W  `  g  s      ��                            ����                            �    ��                    �    �   ��                    �&    undefined                                                               �       ��  �   l   ��  ��                    �����               ��X	                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       �     9          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   ��U	                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  N  Q  L              \�X	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  S  Y  �              @�X	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                  [  \  p              ��W	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  ^  a  p              @�W	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  c  e  �              �&Y	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  g  j  �	              �wY	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  l  m  H              tWX	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  o  p  T              XX	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  r  t  T              x�X	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                  v  w  |              h�U	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  y  z  |              �U	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  |  }  |              L
X	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                    �  |              �
X	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              �X	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �              ��W	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              t�W	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              $�W	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              �X	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              (�W	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `              �7X	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d              �'X	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �              �Y	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              hX	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              p�X	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              ��Y	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              pne	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              <�d	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                  �  �  @$              ��d	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                  �  �  l%               e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                  �  �  d'              ��d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                  �  �  h(              \�d	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                  �  �  �)              e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                  �  �  @+              ��d	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                  �  �  �,              <!e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                       �-              Ĕe	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                      �/              �e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                  
    �0              h�d	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                      �1              x�d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                      �2              ,�d	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3     U       CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	 \       CHARACTER,  canNavigate �3      �3      (4    f       LOGICAL,    closeQuery  4      44      `4   
 r       LOGICAL,    columnProps @4      l4      �4    }       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	 �       CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7    �       LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7    �       LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7    �       CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8    �       CHARACTER,  hasForeignKeyChanged    88      d8      �8    �       LOGICAL,    openDataQuery   |8      �8      �8          LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	 !      LOGICAL,    prepareQuery    9      49      d9    +      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    8      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 E      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	 O      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	 Y      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;    c      CHARACTER,  assignDBRow                             <  �;      ��                  �  �  <              �e	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                  �    L=              �e	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                      �>              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                    
  �?              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                       A              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                      PB              �Ee	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                      PC              �~e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                      PD              �e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                      PE              ,�e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                      PF              0Ne	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                    !  \G              �Ne	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  #  $  �H              �{e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  &  (  �I              �be	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  *  +  �J               �e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  -  .  �K              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  0  3  �L              �e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M    �      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP    �      CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P           CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q           CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !        CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "  *      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  9      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  H      CHARACTER,  getForeignValues    @R      lR      �R  %  W      CHARACTER,  getQueryPosition    �R      �R      �R  &  h      CHARACTER,  getQuerySort    �R      �R      S  '  y      CHARACTER,  getQueryString  �R      (S      XS  (  �      CHARACTER,  getQueryWhere   8S      dS      �S  )  �      CHARACTER,  getTargetProcedure  tS      �S      �S  *  �      HANDLE, indexInformation    �S      �S      T  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .  �      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /  �      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0        CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1        CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2  %      LOGICAL,    removeQuerySelection    �W      �W      (X  3  6      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  K      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
 Y      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6  d      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7  s      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8  �      LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              �.f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\              :f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                  �  �  �]              �<f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                  �  �  �^              @f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                  �  �  `              t@f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                  �  �  a              Df	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                  �  �  b              �Df	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                  �  �  Dc              tIf	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                  �  �  Hd              �Lf	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <  �      CHARACTER,  getASBound  �d      e      0e  = 
 �      LOGICAL,    getAsDivision   e      <e      le  >  �      CHARACTER,  getASHandle Le      xe      �e  ?  �      HANDLE, getASHasStarted �e      �e      �e  @  �      LOGICAL,    getASInfo   �e      �e      f  A 	       CHARACTER,  getASInitializeOnRun    �e       f      Xf  B        LOGICAL,    getASUsePrompt  8f      df      �f  C  "      LOGICAL,    getServerFileName   tf      �f      �f  D  1      CHARACTER,  getServerOperatingMode  �f      �f      g  E  C      CHARACTER,  runServerProcedure  �f      $g      Xg  F  Z      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G  m      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H  {      LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              �f	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl              �f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o              $�f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              �f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u              D�f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                  �  �  �v              $�f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                  �  �  �w              бf	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                  �  �  �x              $�f	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                  �  �  |z              �f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                  �  �  �{              x�f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                  �  �  �|              D�f	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                  �  �  x~              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                  �  �  �              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                  �  �  ��              \�f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                  �     L�              �f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                      t�              |�f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
 :      LOGICAL,    assignLinkProperty  ؃      �      8�  P  E      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  Q  X      CHARACTER,  getChildDataKey ��      ̄      ��  R  f      CHARACTER,  getContainerHandle  ܄      �      <�  S  v      HANDLE, getContainerHidden  �      D�      x�  T  �      LOGICAL,    getContainerSource  X�      ��      ��  U  �      HANDLE, getContainerSourceEvents    ��      ��      ��  V  �      CHARACTER,  getContainerType    ܅      �      <�  W  �      CHARACTER,  getDataLinksEnabled �      H�      |�  X  �      LOGICAL,    getDataSource   \�      ��      ��  Y  �      HANDLE, getDataSourceEvents ��      ��      �  Z  �      CHARACTER,  getDataSourceNames  Ԇ       �      4�  [        CHARACTER,  getDataTarget   �      @�      p�  \  "      CHARACTER,  getDataTargetEvents P�      |�      ��  ]  0      CHARACTER,  getDBAware  ��      ��      �  ^ 
 D      LOGICAL,    getDesignDataObject ȇ      �      (�  _  O      CHARACTER,  getDynamicObject    �      4�      h�  `  c      LOGICAL,    getInstanceProperties   H�      t�      ��  a  t      CHARACTER,  getLogicalObjectName    ��      ��      ��  b  �      CHARACTER,  getLogicalVersion   Ј      ��      0�  c  �      CHARACTER,  getObjectHidden �      <�      l�  d  �      LOGICAL,    getObjectInitialized    L�      x�      ��  e  �      LOGICAL,    getObjectName   ��      ��      �  f  �      CHARACTER,  getObjectPage   ̉      ��      (�  g  �      INTEGER,    getObjectParent �      4�      d�  h  �      HANDLE, getObjectVersion    D�      l�      ��  i        CHARACTER,  getObjectVersionNumber  ��      ��      �  j        CHARACTER,  getParentDataKey    Ċ      ��      $�  k  *      CHARACTER,  getPassThroughLinks �      0�      d�  l  ;      CHARACTER,  getPhysicalObjectName   D�      p�      ��  m  O      CHARACTER,  getPhysicalVersion  ��      ��      �  n  e      CHARACTER,  getPropertyDialog   ȋ      �      (�  o  x      CHARACTER,  getQueryObject  �      4�      d�  p  �      LOGICAL,    getRunAttribute D�      p�      ��  q  �      CHARACTER,  getSupportedLinks   ��      ��      ��  r  �      CHARACTER,  getTranslatableProperties   ��      �      (�  s  �      CHARACTER,  getUIBMode  �      4�      `�  t 
 �      CHARACTER,  getUserProperty @�      l�      ��  u  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    |�      č      ��  v  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ܍      $�      P�  w  	      CHARACTER,INPUT pcLink CHARACTER    linkProperty    0�      t�      ��  x  	      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      ��      �  y  	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      x�      ��  z  *	      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ̏      ��  {  8	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ܏      $�      T�  |  E	      CHARACTER,  setChildDataKey 4�      `�      ��  }  T	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  p�      ��      �  ~  d	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ̐      �      @�    w	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents     �      `�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled |�      ��      ��  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ԑ      �      L�  �  �	      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ,�      l�      ��  �  �	      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      Ȓ      ��  �  �	      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ܒ      $�      T�  �  �	      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 4�      x�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Г      ��  � 
 
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ܓ      �      P�  �  
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    0�      x�      ��  �  -
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      Ȕ       �  �  >
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      $�      \�  �  T
      LOGICAL,INPUT c CHARACTER   setLogicalVersion   <�      x�      ��  �  i
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Е       �  �  {
      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��       �      P�  �  �
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    0�      p�      ��  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ̖       �  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      (�      \�  �  �
      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   <�      |�      ��  �  �
      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ԗ      �  �  �
      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      ,�      \�  �  �
      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   <�      ��      ��  �        LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ܘ      �  �        LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      <�      h�  � 
 4      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  ?      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �  O      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	 [      CHARACTER,INPUT pcName CHARACTER    l�      ��  0�      �       4   �����                 @�                      ��                    G                  �#g	                         Ě          \�  ؛      �       4   �����                 �                      ��                    F                  @$g	                         l�  �    3  �  ��      �       4   �����                 ��                      ��                  ?  A                  �$g	                       ?  �         @                                  ,     
                    � ߱        �  $  C  ��  ���                           $  E  @�  ���                       x                         � ߱        x�    K  ��  �      �      4   �����                �                      ��                  L  	                  x%g	                       L  ��  H�  o   O      ,                                 ��  $   P  t�  ���                       �  @         �              � ߱        ��  �   Q        Ȟ  �   R  �      ܞ  �   T        �  �   V  x      �  �   X  �      �  �   Z  `      ,�  �   [  �      @�  �   \        T�  �   _  �      h�  �   a         |�  �   b  |      ��  �   d  �      ��  �   e  t      ��  �   f  �      ̟  �   g  ,      ��  �   h  �      ��  �   n  �      �  �   p  P	      �  �   v  �	      0�  �   x   
      D�  �   z  t
      X�  �   {  �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  7	  e	  ��              ,=g	                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        h�  $ K	  آ  ���                           O   c	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ  c                     8�    �	  ��  �      t      4   ����t                 �                      ��                  �	  
                  0Bg	                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    <
  T�  Ц      x      4   ����x                �                      ��                  =
  �
                  2f	                       =
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
  y                  �8f	                       �
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
  �8      Ĭ  �      ,9      ج  �     �9      �  �     :       �  �     X:      �  �     �:      (�  �     H;      <�  �     �;      P�  �     8<          �     �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  (                  �*f	                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �    �  ��  ��      K      4   ����K      /   �  а     �                          3   ����K             �                      3   ����<K  Զ    �  ,�  ��  �  XK      4   ����XK  
              ��                      ��             
     �  J                  lig	                       �  <�  ̱  �   �  �K      $�  $  �  ��  ���                       �K     
                    � ߱        8�  �   �  L      ��  $   �  d�  ���                       ,L  @         L              � ߱        L�  $  �  ��  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱        ܳ  V   �  �  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        l�  $    x�  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   +  �  ���                                      ̵                      ��                  L  �                  �jg	                       L  ��  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   a  �  ���                        adm-clone-props �  ��              �     W     `                          \  ]                     start-super-proc    �  d�  �           �     X                                  ~                     l�      �   �      �X      4   �����X      /     ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $    ��  ���                       @Y                         � ߱        ��    ,  �  \�  ��  \Y      4   ����\Y                и                      ��                  -  1                  �fg	                       -  �  pY                     �Y                     �Y                         � ߱            $  .  l�  ���                             2  �  T�      �Y      4   �����Y  �Y                         � ߱            $  3  (�  ���                       |�    :  ��  ��  �  �Y      4   �����Y      $  ;  ع  ���                       Z                         � ߱            �   X  Z      XZ     
                �Z                     $\  @        
 �[              � ߱        ��  V   l  �  ���                        ��  �   �  0\      ��      غ  �      p\      4   ����p\      /     �     $�                          3   �����\            D�                      3   �����\  �\     
                <]                     �^  @        
 L^              � ߱        H�  V   +  T�  ���                        �^     
                T_                     �`  @        
 d`              � ߱        t�  V   O  �  ���                        ��    �  ��  �      �`      4   �����`                �                      ��                  �  �                  �g	                       �  ��  ��  /   �  H�     X�                          3   �����`            x�                      3   �����`      /   �  ��     Ľ                          3   ����a            �                      3   ����$a   �  /  <   �         Xa                      3   ����@a  initProps   x�  0�                   Y     �             �          �  �  	                                   t�          �  �      ��                �  �  4�              �h	                    O   ����    e�          O   ����    R�          O   ����    ��      �                      L�          ��  p   �  ,|  ��      �  ��  �     8|                �                      ��                  �  �                  %h	                       �  ��  4�  :  �                 $  �  `�  ���                       L|                         � ߱        �  �     d|                                        ��                  �  �                  �h	                       �  ��  ��  ��     x|                                        ��                  �                     xh	                       �  (�  0�   �     �|                                        ��                                      H h	                         ��  ��  ��     �|                                        ��                    :                  !h	                         @�  H�  8�     �|                                        ��                  ;  W                  (&h	                       ;  ��  ��  ��     �|                                        ��                  X  t                  �&h	                       X  X�  `�  P�     �|                                        ��                  u  �                  �'h	                       u  ��  ��  ��     �|  	                                      ��             	     �  �                  �(h	                       �  p�  x�  h�     }  
                                      ��             
     �  �                  �)h	                       �  ��  �  ��     }                                        ��                  �  �                  p*h	                       �  ��  ��  ��     ,}                                        ��                  �                    @+h	                       �  �  �  �     @}                                        ��                    "                  ,h	                         ��  ��  ��     T}                                        ��                  #  ?                  �,h	                       #  ,�  4�  $�     h}                                        ��                  @  \                  �-h	                       @  ��  ��  ��     |}                                        ��                  ]  y                  �.h	                       ]  D�  L�  <�     �}                                        ��                  z  �                  �/h	                       z  ��      ��     �}                                        ��                  �  �                  \0h	                       �  \�      O   �  ��  ��  �}               \�          D�  P�   , $�                                                       �     ��                            ����                            <�  d�  X�  ��      ��     Z     d�                      � `�  �                     ��    �  �  ��      �}      4   �����}                ��                      ��                  �  �                  ��W	                       �  ,�  �  /   �  ��     ��                          3   �����}            �                      3   �����}  ��  /   �  @�     P�                          3   ����~            p�                      3   ����,~  ��  /   �  ��     ��                          3   ����H~            ��                      3   ����h~      /   �  �     (�                          3   �����~            H�                      3   �����~  �~     
                D                     ��  @        
 T�              � ߱        ��  V   ,  X�  ���                        ��  $  F  �  ���                       ��                         � ߱        Ȁ     
                D�                     ��  @        
 T�              � ߱        ��  V   P  @�  ���                        ��  $  j  ��  ���                       ��     
                    � ߱        ��     
                0�                     ��  @        
 @�              � ߱        ��  V   t  (�  ���                        t�  $  �  ��  ���                       ��     
                    � ߱        ��     
                �                     l�  @        
 ,�              � ߱        ��  V   �  �  ���                        \�  $  �  ��  ���                       ��                         � ߱        ��     
                (�                     x�  @        
 8�              � ߱        ��  V   �  ��  ���                        ��  �   �  ��      X�  $  �  ��  ���                       ��     
                    � ߱        Ĉ     
                @�                     ��  @        
 P�              � ߱        ��  V   �  ��  ���                        ��  $  �  ��  ���                       ��     
                    � ߱        ��  �     ��      H�  $     �  ���                       ��     
                    � ߱        \�  �   :  �      ��  $  \  ��  ���                       D�                         � ߱              g  ��  ��      `�      4   ����`�      /   h  �     �                          3   ������  L�     
   <�                      3   ������  |�        l�                      3   ������  ��        ��                      3   ������            ��                      3   ����؋  pushRowObjUpdTable  ��  ��  �                   [      �                                                    pushTableAndValidate    ��  L�  �           |     \     �                          �  5                     remoteCommit    d�  ��  �           p     ]     �                          �  �                     serverCommit    ��  ,�  �           l     ^     �                          �  �                                     L�          �  �      ��                  �  �  4�              �gh	                    O   ����    e�          O   ����    R�          O   ����    ��          O   �  ��  ��  �    ��                            ����                            <�  P�      ��              _      d�                      
�     �                     disable_UI  ��   �                      `      �                               �  
                    �  �    ����  �       ��          ��  8   ����   ��  8   ����   ��  8   ����   ��  8   ����       8   ����       8   ����        �  �      viewObject  ,   ��   �  ,�      toggleData  ,INPUT plEnabled LOGICAL    �  X�  p�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  H�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  8�  D�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE (�  ��  ��      removeAllLinks  ,   ��  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  $�  8�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    �  ��  ��      hideObject  ,   ��  ��  ��      exitObject  ,   ��  ��  �      editInstanceProperties  ,   ��  �  ,�      displayLinks    ,   �  @�  P�      createControls  ,   0�  d�  t�      changeCursor    ,INPUT pcCursor CHARACTER   T�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  @�  L�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER 0�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��   �  �      unbindServer    ,INPUT pcMode CHARACTER ��  8�  H�      runServerObject ,INPUT phAppService HANDLE  (�  t�  ��      disconnectObject    ,   d�  ��  ��      destroyObject   ,   ��  ��  ��      bindServer  ,   ��  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  4�  @�      startFilter ,   $�  T�  d�      releaseDBRow    ,   D�  x�  ��      refetchDBRow    ,INPUT phRowObjUpd HANDLE   h�  ��  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE ��  ��  �      fetchDBRowForUpdate ,   ��  $�  4�      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL �  d�  t�      compareDBRow    ,   T�  ��  ��      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   x�  �  �      assignDBRow ,INPUT phRowObjUpd HANDLE    �  H�  T�      updateState ,INPUT pcState CHARACTER    8�  ��  ��      updateQueryPosition ,   p�  ��  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    ��   �  �      undoTransaction ,   ��  $�  4�      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  �  ��  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   ��  ,�  @�      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   �  ��  ��      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  |�  �  $�      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  �  h�  |�      startServerObject   ,   X�  ��  ��      setPropertyList ,INPUT pcProperties CHARACTER   ��  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��   �  0�      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    �  ��   �      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  ,�  <�      rowObjectState  ,INPUT pcState CHARACTER    �  h�  x�      retrieveFilter  ,   X�  ��  ��      restartServerObject ,   |�  ��  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   ��  ��  ��      refreshRow  ,   ��  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  P�  `�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  @�  ��  ��      initializeServerObject  ,   ��  ��  ��      initializeObject    ,   ��  ��  ��      home    ,   ��   �  �      genContextList  ,OUTPUT pcContext CHARACTER ��  <�  H�      fetchPrev   ,   ,�  \�  h�      fetchNext   ,   L�  |�  ��      fetchLast   ,   l�  ��  ��      fetchFirst  ,   ��  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   ��  ��  �      endClientDataRequest    ,   ��   �  4�      destroyServerObject ,   �  H�  X�      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    8�  ��  ��      dataAvailable   ,INPUT pcRelative CHARACTER ��  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  4�  H�      commitTransaction   ,   $�  \�  l�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    L�  �  �      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 W	%     adecomm/as-utils.w 
"   
   �    }        �
"     
    �     }        ��    ;   %               � 
" 	   
 �%              h �P  \         (          
�                          
�            � e   R	
" 	   
 g	
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 �               1� u  
 � �   �%               o%   o           � �    
"   
 �           �    1� �   � �   �%               o%   o           � �   
"   
 �           �    1� �  
 � �   �%               o%   o           � �   
"   
 �           l    1� �   � �   �%               o%   o           � �    
"   
 �           �    1� �   � �   �%               o%   o           � �   
"   
 �           T    1� �   � �   �%               o%   o           %               
"   
 ��          �    1� �   ��      
"   
 �               1�    � �   �%               o%   o           � &  
"   
 �           �    1� (   � �   �%               o%   o           � 7  S 
"   
 �           �    1� �   � �   �%               o%   o           %               
"   
 �           p    1� �   � �   �%               o%   o           %               
"   
 �           �    1� �   � �   �%               o%   o           %              
"   
 ��          h    1� �   �� �     
"   
 �           �    1� �  
 � �   �%               o%   o           %               
"   
 �                1� �   � �   �%               o%   o           � �    
"   
 ��          �    1� �   ��      
"   
 �           �    1� �   � �   �%               o%   o           �   t 
"   
 ��          D	    1� w  
 ��      
"   
 �           �	    1� �   � �   �%               o%   o           � �  � 
"   
 �           �	    1�     � �   �%               o%   o           � �    
"   
 �           h
    1� 7  
 � B   �%               o%   o           %               
"   
 g	�           �
    1� F   g	� �   �%               o%   o           %              
"   
 g	�           `    1� N   g	� �   �%               o%   o           � �    g	
"   
 g	�           �    1� _   g	� �   �%               o%   o           o%   o           
"   
 g	�           P    1� o  
 g	� �   �%               o%   o           � �    g	
"   
 g	�           �    1� z   g	� �  	 �%               o%   o           � �  / g	
"   
 ��          8    1� �   �� �  	   
"   
 g	�           t    1� �   g	� �  	 �o%   o           o%   o           � �    g	
"   
 ��          �    1� �   �� �  	   
"   
 g	�           $    1� �   g	� �  	 �o%   o           o%   o           � �    g	
"   
 ��          �    1� 	   �� �     
"   
 ��          �    1�    �� �  	   
"   
 ��              1� $   �� �  	   
"   
 ��          L    1� 1   �� �  	   
"   
 g	�           �    1� ?   g	� �   �o%   o           o%   o           %              
"   
 ��              1� P   �� �  	   
"   
 ��          @    1� ^  
 �� i     
"   
 ��          |    1� q   �� �  	   
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
 g	
"   
   
"   
 R	(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�            ��       p�               �L
�    %              � 8          � $         �           
�    � !     
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 g	�           �    1� $  
 g	� �   �%               o%   o           � �    g	
"   
 g	�           <    1� /  
 g	� �   �%               o%   o           o%   o           
"   
 g	�           �    1� :   g	�    �%               o%   o           o%   o           
"   
 g	�           4    1� C   g	� �   �%               o%   o           %               
"   
 g	�           �    1� R   g	� �   �%               o%   o           %               
"   
 W	�           ,    1� _   W	� �   �%               o%   o           � �    g	
"   
 g	�           �    1� f   g	� �   �%               o%   o           %              
"   
 g	�               1� x   g	� �   �%               o%   o           o%   o           
"   
 g	�           �    1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�               1� �  	 g	� �   �%               o%   o           � �    g	
"   
 g	�           �    1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�               1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�           �    1� �   g	� �   �%               o%   o           %               
"   
 g	�           �    1� �   g	� �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 g	�           �    1� �  
 g	� �   �%               o%   o           %              
"   
 g	�           H    1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�           �    1� �   g	� �   �%               o%   o           � �    g	
"   
 g	�           8    1�     g	� �   �%               o%   o           o%   o           
"   
 ��          �    1�    ��      
"   
 g	�           �    1�    g	� �   �%               o%   o           � ,  ! g	
"   
 g	�           d    1� N   g	� �   �%               o%   o           � �    g	
"   
 g	�           �    1� [   g	� �   �%               o%   o           � n   g	
"   
 ��          L    1� }   �� �     
"   
 ��          �    1� �   ��      
"   
 g	�           �    1� �   g	� �   �%               o%   o           � �    g	
"   
 ��          8     1� �  
 ��      
"   
 W	�           t     1� �   W	� �   �%               o%   o           o%   o           
"   
 g	�           �     1� �   g	� �   �%               o%   o           %               
"   
 g	�           l!    1� �   g	� �   �%               o%   o           %               
"   
 g	�           �!    1� �   g	� �   �%               o%   o           � �    g	
"   
 g	�           \"    1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�           �"    1�    g	� �   �%               o%   o           %              
"   
 g	�           T#    1�    g	� �   �%               o%   o           %               
"   
 W	�           �#    1� !   W	� �   �%               o%   o           %               
"   
 ��          L$    1� 1   ��      
"   
 ��          �$    1� >   �� �     
"   
 g	�           �$    1� K   g	� B   �%               o%   o           o%   o           
"   
 g	�           @%    1� W   g	� �   �%               o%   o           � �    g	
"   
 g	�           �%    1� e   g	� �   �%               o%   o           o%   o           
"   
 g	�           0&    1� s   g	� �   �o%   o           o%   o           o%   o           
"   
 g	�           �&    1� �   g	� �  	 �%               o%   o           o%   o           
"   
 g	�           ('    1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�           �'    1� �  
 g	� B   �%               o%   o           o%   o           
"   
 ��           (    1� �   �� �     
"   
 g	�           \(    1� �   g	� �   �%               o%   o           � �  4 g	
"   
 g	�           �(    1�   
 g	� �   �%               o%   o           %              
"   
 ��          L)    1�    ��      
"   
 g	�           �)    1� *   g	� �   �%               o%   o           � �    W	
"   
 g	�           �)    1� 8   g	� �   �%               o%   o           %              
"   
 g	�           x*    1� G   g	� �   �%               o%   o           � �    g	
"   
 g	�           �*    1� T   g	� �   �%               o%   o           � �    g	
"   
 g	�           `+    1� b   g	� �   �%               o%   o           � �    g	
"   
 g	�           �+    1� n   g	� �   �%               o%   o           %               
"   
 g	�           P,    1� }  	 g	�    �%               o%   o           o%   o           
"   
 W	�           �,    1� �   W	� �   �%               o%   o           � �  	 g	
"   
 g	�           @-    1� �   g	� B   �%               o%   o           %       �       
"   
 g	�           �-    1� �   g	� �   �%               o%   o           � �    g	
"   
 g	�           0.    1� �   g	� �   �o%   o           o%   o           %              
"   
 g	�           �.    1� �   g	� �   �%               o%   o           %               
"   
 g	�           (/    1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�           �/    1� �   g	� �  	 �%               o%   o           � �    g	
"   
 ��          0    1� �   �� �  	   P �L 
�H T   %              �     }        �GG %              
"   
 W	�           �0    1�   
 W	� �   �%               o%   o           � �    W	
"   
 g	�           1    1�    g	� �   �%               o%   o           %               
"   
 g	�           �1    1� #  	 g	� �   �%               o%   o           � �    g	
"   
 g	�           2    1� -   g	� �   �%               o%   o           � �    g	
"   
 g	�           �2    1� ;   g	� �   �%               o%   o           %               
"   
 g	�           �2    1� K   g	� �   �%               o%   o           � �    g	
"   
 g	�           p3    1� ^   g	� �   �%               o%   o           o%   o           
"   
 g	�           �3    1� f   g	� �   �%               o%   o           o%   o           
"   
 g	�           h4    1� s   g	� �   �%               o%   o           o%   o           
"   
 W	�           �4    1� �   W	� �   �%               o%   o           o%   o           
"   
 g	�           `5    1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�           �5    1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�           X6    1� �  	 g	� �  	 �%               o%   o           � �    g	
"   
 g	�           �6    1� �  
 g	� �  	 �%               o%   o           � �    g	
"   
 g	�           @7    1� �   g	� �   �%               o%   o           � �    g	
"   
 g	�           �7    1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�           08    1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�           �8    1� �   g	� �   �%               o%   o           � �    g	
"   
 g	�            9    1�    g	� �   �%               o%   o           � �    g	
"   
 g	�           �9    1�    g	� �  	 �%               o%   o           o%   o           
"   
 ��          :    1� &   ��      
"   
 g	�           L:    1� 2   g	� �   �%               o%   o           � �    g	
"   
 g	�           �:    1� @   g	� �   �%               o%   o           o%   o           
"   
 W	�           <;    1� S   W	� �   �%               o%   o           o%   o           
"   
 g	�           �;    1� e  
 g	� �   �%               o%   o           � �    g	
"   
 g	�           ,<    1� p   g	� �   �%               o%   o           � �    g	
"   
 g	�           �<    1� �   g	� �   �%               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 g	�           p=    1� �  	 g	�    �%               o%   o           o%   o           
"   
 g	�           �=    1� �   g	�    �%               o%   o           o%   o           
"   
 g	�           h>    1� �   g	�    �%               o%   o           o%   o           
"   
 W	�           �>    1� �   W	� �   �%               o%   o           %              
"   
 g	�           `?    1� �   g	� �   �%               o%   o           � �  M W	
"   
 g	�           �?    1� <   g	� �   �%               o%   o           %              
"   
 g	�           P@    1� M   g	� �   �%               o%   o           %               
"   
 g	�           �@    1� a   g	� �   �%               o%   o           %               
"   
 g	�           HA    1� x   g	� �  	 �%               o%   o           � �   g	
"   
 g	�           �A    1� �   g	� �   �%               o%   o           %               
"   
 g	�           8B    1� �   g	� �  	 �%               o%   o           o%   o           
"   
 g	�           �B    1� �   g	� �   �o%   o           o%   o           %              
"   
 W	�           0C    1� �   W	� �  	 �o%   o           o%   o           � �    W	
"   
 g	�           �C    1� �   g	�    �o%   o           o%   o           o%   o           
"   
 g	�            D    1� �   g	�    �o%   o           o%   o           o%   o           
"   
 g	�           �D    1� �   g	� �  	 �o%   o           o%   o           o%   o           
"   
 g	�           E    1�    g	�    �o%   o           o%   o           o%   o           
"   
 g	�           �E    1�    g	� �  	 �o%   o           o%   o           � %   g	
"   
 g	�           F    1� '   g	� �  	 �o%   o           o%   o           � 6   g	
"   
 g	�           |F    1� B   g	� �   �%               o%   o           %               
"   
 g	�           �F    1� V   g	� �   �%               o%   o           %               
"   
 ��          tG    1� j   �� �  	   
"   
 g	�           �G    1� ~   g	� �   �%               o%   o           %               
"   
 g	�           ,H    1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�           �H    1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�           $I    1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�           �I    1� �   g	� �   �%               o%   o           � �    g	
"   
 g	�           J    1� �   g	� �   �%               o%   o           %               
"   
 g	�           �J    1� �  	 g	� �   �%               o%   o           %                "    �%     start-super-proc u�%     adm2/smart.p �R	P �L 
�H T   %              �     }        �GG %              
"   
   �       �K    6� �     
"   
   
�        �K    8
"   
   �        �K    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �        �M    �� �   � P   �        �M    �@    
� @  , 
�       �M    ��     R	p�               �L
�    %              � 8       N    � $         �           
�    � !   R	
"   
 �p� @  , 
�       O    ��    �p�               �L"  	  , �   �    g	� !   ��     }        �A      |    "  	    �    g	%              (<   \ (    |    �     }        �A� #   �A"  
  g	    "  	  R	"  
  g	  < "  	  R	"  
  g	(    |    �     }        �A� #   �A"  
  g	
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �        �P    �� �   � P   �        �P    �@    
� @  , 
�       �P    ��     R	p�               �L
�    %              � 8      Q    � $         �           
�    � !   R	
"   
 �p� @  , 
�       R    �� u  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 g	
"   
   
"   
   (�  L ( l       �        �R    �� �   � P   �        �R    �@    
� @  , 
�       �R    ��       p�               �L
�    %              � 8      �R    � $         �           
�    � !     
"   
 �p� @  , 
�       �S    �� �  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�       XT    �� �     p�               �L%               
"   
  p� @  , 
�       �T    �� �    p�               �L%               
"   
  p� @  , 
�       U    �� �    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 g	 (   � 
"   
 R	    �        �U    �� �   �
"   
   � 8      DV    � $         �           
�    � !   R	
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6� �     
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    � L   g	
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 R	    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 g	"      �       }        �
"   
 �%              %                "    �%     start-super-proc u�%     adm2/appserver.p Ug	�    � �     
�    �     }        �%               %      Server  - �     }        �    "    g	� �    �%                   "    g	� �    �%      NONE    p�,  8         $     "    g	        � �   R	
�    
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �        �Z    �� �   � P   �        �Z    �@    
� @  , 
�       �Z    ��     R	p�               �L
�    %              � 8      �Z    � $         �           
�    � !   R	
"   
 �p� @  , 
�       �[    �� �   �p�               �L"    , p�,  8         $     "    g	        � �   R	
�     "    �%     start-super-proc t�%     adm2/dataquery.p �W	
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
 R	(�  L ( l       �        ]    �� �   � P   �        ]    �@    
� @  , 
�       $]    ��     R	p�               �L
�    %              � 8      0]    � $         �    R	     
�    � !   R	
"   
 �p� @  , 
�       @^    �� �   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
 R	(�  L ( l       �        $_    �� �   � P   �        0_    �@    
� @  , 
�       <_    ��     R	p�               �L
�    %              � 8      H_    � $         �    R	     
�    � !   R	
"   
 �p� @  , 
�       X`    �� �   �p�               �L%               "    �%     start-super-proc s�%     adm2/query.p �R	%     start-super-proc s�%     adm2/queryext.p % 	    initProps R	
�    %8 , (   FOR EACH CcbCDocu NO-LOCK INDEXED-REPOSITION �R	�   � c     � e     �       
�     	         �G
"   
 g	�        �a    �G
"   
   
"   
    x    (0 4      �        �a    �G%                   �        b    �GG %              � I    R	� J         %              %                   "      %              
"   
       "      �        �b    �
"   
   �        ,c    �
"   
   
�       Lc    �"       \      H   "    R	((       "      %              � �      � c   R	     
"   
   
"   
 � \      H   "      ((       "      %              � �     � c   g	�        �c    �%                   %              %                   "  (    %                  "  (        
"   
 R	
"   
 g	0 T       m � "  (  W	�        �d    �A @   "      $         � "  (  g	� #   ��        e    �� "  (    
"   
 � \ H     H   "      ((       "    R	%              � �    �� c     (        "  !  R	� �    g	�        �e    �"  !  �
�H T   %              �     }        �GG %              
"   
 g	
"   
 g	
"   
   
"   
   (�  L ( l       �        �f    �� �   � P   �        �f    �@    
� @  , 
�       �f    ��       p�               �L
�    %              � 8      �f    � $         �           
�    � !     
"   
 �p� @  , 
�       �g    �� �   �p�               �L%               
"   
   p� @  , 
�       Hh    �� �     p�               �L"    , �,  8         $     "    �L        � Q  
  
�    
�H T   %              �     }        �GG %              
"   
 $ 
"   
   
"   
 R	
"   
 R	(�  L ( l       �        ,i    �� �   � P   �        8i    �@    
� @  , 
�       Di    ��     R	p�               �L
�    %              � 8      Pi    � $         �    R	     
�    � !     
"   
 �p� @  , 
�       `j    �� &   �p�               �L
"   
 , 
"   
   p� @  , 
�       �j    ��      p�               �L"    , 
"   
  p� @  , 
�       k    �� �    p�               �L"    ,     "    g	� �    �%L C <   OPEN QUERY Query-Main FOR EACH CcbCDocu NO-LOCK INDEXED-REPOSITION.     "    CD� �   K ((        "    PO%                   "    �� �     "    R	 (   "           "    �%              @ �,  8         $     "    R	        � �    
�    p�,  8         $     � �   g	        � �   R	
�    %               �    "      � e         %              %                   "      %                  "      "      "     T(        "    g	%              "    g	� e   �"      �       "    R	�    "    g	� #   �� �      � #   R	�    "     � #    S    "      "    �    "    g	%                � @    �     t T     P   4       �"      (0       4       g	"      � �      � �    R	� c   g	T ,  %              T   "    g	"    �� e     � #   R	� c   g	T    �    "    g	� #   �"      � #   R	"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "    R	%              � �    �� �     4  g	     "      
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �        �q    �� �   � P   �        �q    �@    
� @  , 
�       �q    ��     R	p�               �L
�    %              � 8      �q    � $         �           
�    � !   R	
"   
 �p� @  , 
�       �r    ��   
 �p�               �L"    ,       "  
  g	�    � �   g	� e   �      "  	    �    � �   �� e   g	�   � c     � e     � �   R	�   � c     � e   R	� �   g	�   � c     � e     � �     
�H T   %              �     }        �GG %              
"   
 �
"   
 R	
"   
 �
"   
 �(�  L ( l       �        Ht    �� �   � P   �        Tt    �@    
� @  , 
�       `t    ��     �p�               �L
�    %              � 8      lt    � $         �           
�    � !     
"   
 �p� @  , 
�       |u    �� �   �p�               �L"    , 
"   
   p� @  , 
�       �u    �� p     p�               �L"    , 
"   
  p� @  , 
�       ,v    �� K    p�               �L"    ,     %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    � �     � e         "  	  g	�     "    g	T    "      "      @ A,    �   � c   �� �     "    R	"       T      @   "    �(        "      � �    R	� �      � c   R	"    g	     "  	   %              D H   @ A,    �   � c   R	� �     "    R	"    g	,    S   "    R	� �   g	� e   �%                T      @   "    �(        "      � �    R	� �      � c   R	"    g	     "  
   %                         "    �� �     "    R	           "      � �   R	"      
�H T   %              �     }        �GG %              
"   
 h	
"   
   
"   
 h	
"   
 R	(�  L ( l       �        Hz    �� �   � P   �        Tz    �@    
� @  , 
�       `z    ��     h	p�               �L
�    %              � 8      lz    � $         �    R	     
�    � !   �
"   
 �p� @  , 
�       |{    �� p   �p�               �L"    , 
"   
   p� @  , 
�       �{    �� K     p�               �L"    , "      %               �     }        �%              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    �%     start-super-proc r�%     adm2/data.p %     start-super-proc r�%     adm2/dataext.p 	%     start-super-proc r�%     adm2/dataextcols.p 	%     start-super-proc r�%     adm2/dataextapi.p g	
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
 R	(�  L ( l       �            �� �   � P   �             �@    
� @  , 
�       ,    ��     R	p�               �L
�    %              � 8      8    � $         �    R	     
�    � !   R	
"   
 �p� @  , 
�       H�    �� ~   �p�               �L%               %     "Util/dccbcdocu.i"  
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �        �    �� �   � P   �         �    �@    
� @  , 
�       ,�    ��     R	p�               �L
�    %              � 8      8�    � $         �           
�    � !   R	
"   
 �p� @  , 
�       H�    �� x   �p�               �L"    , 
�     	        �G
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �         �    �� �   � P   �        �    �@    
� @  , 
�       �    ��     R	p�               �L
�    %              � 8      $�    � $         �           
�    � !   R	
"   
 �p� @  , 
�       4�    �� �  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �        �    �� �   � P   �        ��    �@    
� @  , 
�       �    ��     R	p�               �L
�    %              � 8      �    � $         �           
�    � !   R	
"   
 �p� @  , 
�        �    �� }  	 �p�               �L
"   
 , 
"   
 �     � �  	   �        x�    �
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �        ��    �� �   � P   �        �    �@    
� @  , 
�       �    ��     R	p�               �L
�    %              � 8      �    � $         �           
�    � !   R	
"   
 �p� @  , 
�       ,�    �� �   �p�               �L"    , 
"   
   �       ��    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�       (�    ��     R	p�               �L
�    %              � 8      4�    � $         �           
�    � !   R	
"   
 �p� @  , 
�       D�    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 g	        � �   R	
�    
�             �Gp�,  8         $     
"   
 g	        � �   R	
�    �    � �     
�        "    g	� �    �%     modifyListProperty 
�    %      REMOVE  %     SupportedLinks 	%     Update-Target  	%     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � i     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           �   l       ��                 Y  }  �               �mg	                    O   ����    e�          O   ����    R�          O   ����    ��        $  h  �   ���                       �U     
                    � ߱              i  (  �      V      4   ����V                �                      ��                  j  |                  �og	                       j  8  �  �  k  PV            m  �  `      �V      4   �����V                p                      ��                  n  {                  �g	                       n  �  �  o   o      ,                                 �  �   p  �V      �  �   q  �V      $  $  r  �  ���                        W     
                    � ߱        8  �   s  @W      L  �   t  `W      `  �   w  �W          $   z  �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               ̜g	                    O   ����    e�          O   ����    R�          O   ����    ��      m                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  L�g	                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �   �  �X          O   �  ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��            	     H  �  �                �g	                    O   ����    e�          O   ����    R�          O   ����    ��        $  h  �   ���                       `a                         � ߱        �  $  i  8  ���                       �a                         � ߱        �a     
                b  @         �a              � ߱        D  $   �  d  ���                         T      �  x                      ��        0         �  �                  ��g	      8c     L     �  �      $  �  �  ���                       �b                         � ߱          $  �  �  ���                       �b                         � ߱            4   ����c  Xc     
                xc                     �c                         � ߱          $  �    ���                                                            ��                  �  �                  T�g	                �     �  �  �  $  �  L  ���                       td       !       !           � ߱          �      L  �                      ��        0         �  �                  D�g	     ( �d            �  x      $  �     ���                       �d       (       (           � ߱        �  $  �  x  ���                       �d       (       (           � ߱            4   �����d        �  �  L      e      4   ����e                \                      ��                  �  �                  L�g	                       �  �  �  $  �  �  ���                       �e       !       !           � ߱            O   �  �� ��          $  �  �  ���                       �e                         � ߱        hf     
                �f                     4h  @        
 �g          �h  @        
 Th          �h                     �h     
                \i                     �j  @        
 lj          k  @        
 �j          \k  @        
 k              � ߱        x  V   �  $  ���                        P	    |  �  $	      hk      4   ����hk  �k                     �k                     �k                     Tl                         � ߱            $  }  �  ���                       �	    �  l	  |	      �l      4   �����l      �   �  �l      �	  $  �  �	  ���                       m                         � ߱        �
  $  �  
  ���                       (m                         � ߱          �
                              ��        0         �  �                  4h	      �m     �     �  @
      $  �  �
  ���                       Hm                         � ߱        l  $  �  @  ���                       xm                         � ߱            4   �����m  �m                     n                      n                     pn                     �n                         � ߱        D  $  �  |  ���                             �  `  p      �n      4   �����n      $  �  �  ���                       �n          p             � ߱        �  $  �  �  ���                       p                         � ߱          �      �  \                      ��        0         �  �                  ,h	      �p          �         $  �  �  ���                       $p                         � ߱        L  $  �     ���                       Tp                         � ߱            4   ����|p      $  �  �  ���                       �p                         � ߱        8q     
                �q                     s  @        
 �r              � ߱        �  V   �  �  ���                        s       
       
       Ds       	       	       xs                     �s                         � ߱        �  $    D  ���                       �  $  �    ���                       �s                         � ߱        �s     
                xt                     �u  @        
 �u           v  @        
 �u          xv  @        
 8v              � ߱        |  V   �  H  ���                          �      �  \                      ��        0    	     &  ;                  �h	      w     4     &        $  &  �  ���                       �v                         � ߱        <  $  &    ���                       �v                         � ߱        L  4   �����v      4   ����w  �  $  +  �  ���                       |w                         � ߱        �    -  �  L      �w      4   �����w                �                      ��                  .  2                  �h	                       .  �  �w                     Hx       	       	           � ߱            $  /  \  ���                             4  �  h      px      4   ����px  	              �                      ��             	     6  :                  8h	                       6  �  y                     ly       
       
           � ߱            $  7  x  ���                       �y                     �y                         � ߱        �  $  A  �  ���                       �y     
                xz                     �{  @        
 �{           |  @        
 �{              � ߱            V   O  `  ���                                    J `          �  �  � X@                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
             
                                                                                                                                                                                                                               ) �   �   �   �       (  8  H  X  h  x  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX     ) �   �   �   �      (  8  H  X  h  x  �  �  �  �  �   �     (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX  �   :                  � �                     �    ��                      ��                            ����                            �                          �                                �   l       ��                  �    �               lQh	                    O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   l       ��                      �               �Xh	                    O   ����    e�          O   ����    R�          O   ����    ��      +       �              �                  $                  d  /    $     4  �                      3   �����            T                      3   �����      O     ��  ��  $�               �          �  �    �                                             ��                            ����                                            H          �   l       ��                  (  S  �               XUh	                    O   ����    e�          O   ����    R�          O   ����    ��      J       �              �                $                  T       ,             �          _                                �  /  G  t     �  L�                      3   ����(�            �                      3   ����T�     /  I  �     �  |�                      3   ����`�  x                             3   ������      $   I  L  ���                                                   � ߱                  �  �                  3   ������      $   I  �  ���                                                   � ߱        X  $  M  ,  ���                       ��                         � ߱            O   Q  ��  ��  ��               �          �  �   @ �                                                              0              0           ��                            ����                                                       �   l       ��                  ]  ~  �               lah	                    O   ����    e�          O   ����    R�          O   ����    ��      �       $                  T                    �          _                      �              /  {  L     \  �                      3   ����̌  �        |  �                  3   ������      $   {  �  ���                                                   � ߱                                      3   ������      $   {  @  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   l       ��                  �  �  �               `mh	                    O   ����    e�          O   ����    R�          O   ����    ��            �  �   �       �      4   �����      �   �  0�    ��                            ����                            TXS appSrvUtils CcbCDocu O:\on_in_co\Util\dccbcdocu.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "Util/dccbcdocu.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH CcbCDocu NO-LOCK INDEXED-REPOSITION ,   hQuery cOpenQuery cDBNames cPhysicalTables cKeyTableEntityFields cKeyTableEntityValues cKeyTableEntityMnemonic cKeyTableEntityObjField cDBName cEntityFields lHasObjectField lHasAudit lHasComment lHasAutoComment iLookup iAlias  STATIC setDBNames OPEN QUERY Query-Main FOR EACH CcbCDocu NO-LOCK INDEXED-REPOSITION.  FOR   PRESELECT  setOpenQuery 5 showMessage ; NroDoc Query-Main INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI qDataQuery   �/  @  �=      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
   hBuffer �        �     cOpenQuery          �     cDBNames    (             cPhysicalTables T        <     cKeyTableEntityFields   �        h     cKeyTableEntityValues   �        �     cKeyTableEntityMnemonic �         �     cKeyTableEntityObjField �     !   �     cDBName      "        cEntityFields   <     #   ,     lHasObjectField \     $   P     lHasAudit   |     %   p     lHasComment �     &   �     lHasAutoComment �     '   �     iLookup        (   �     iAlias  |    3   Y   �                            initProps   h  i  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  |  }  �  �  �  �  �  �  �  �  �  �  �  �  �  �    �  �  &  +  -  .  /  2  4  6  7  :  ;  A  O  �            �     lRet                      piTableIndex    �  h  *   Z   �  �      T                  deleteRecordStatic  �  �  �  �  �  �  �  �           :  ;  W  X  t  u  �  �  �  �  �  �  �  �      "  #  ?  @  \  ]  y  z  �  �  �  �  �  �                 !       $  l     [             X                  pushRowObjUpdTable    �        �        pcValType                  $       (  �     \       p      �                  pushTableAndValidate          $                pcContext   <             $       `        T        pcMessages            x        pcUndoIds   �  �     ]              �                  remoteCommit    G  I  M  Q  S  �             $                       pcMessages            ,        pcUndoIds   �  x     ^       �      h                  serverCommit    {  ~  8  �     _               �                  getRowObjUpdStatic  �  �  �       `               �                  disable_UI  �  �  �  �   
     �      �                       �  P  \     RowObject   �         �         �         �         �         �         NroDoc  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     �  �     RowObjUpd   P         X         `         l         t         �         �         NroDoc  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   �          �  
   appSrvUtils �       �     xiRocketIndexLimit          �  
   gshAstraAppserver   4           
   gshSessionManager   X        H  
   gshRIManager    �        l  
   gshSecurityManager  �        �  
   gshProfileManager   �  	 	     �  
   gshRepositoryManager       
 
     �  
   gshTranslationManager   $          
   gshWebManager   H        8     gscSessionId    l        \     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager   �        �  
   gshAgnManager   �        �     gsdTempUniqueID              gsdUserObj  D        0     gsdRenderTypeObj    l        X     gsdSessionScopeObj  �       �  
   ghProp  �       �  
   ghADMProps  �       �  
   ghADMPropsBuf   �       �     glADMLoadFromRepos              glADMOk 0       $  
   ghContainer P    	   D     cObjectName l    
   d     iStart  �       �     cAppService �       �     cASDivision �       �     cServerOperatingMode    �       �     cContainerType                 cQueryString    @        4   
   hRowObject  `        T   
   hDataQuery  �        t      cColumns             �      cDataFieldDefs  �        �   CcbCDocu    �     X  �   RowObject         X  �   RowObjUpd            9   �   �   �   �           3  ?  @  A  C  E  F  G  K  L  O  P  Q  R  T  V  X  Z  [  \  _  a  b  d  e  f  g  h  n  p  v  x  z  {  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
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
                     y  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  (  �  �  �  �  �  �  �  �  �  �    +  J  L  a  �        ,  -  .  1  2  3  :  ;  X  l  �      +  O  �  �  �  �  �  <  �  �  �  �  �  �  �  ,  F  P  j  t  �  �  �  �  �  �  �  �       :  \  g  h      ��  C:\Progress\OpenEdge\src\adm2\data.i  %  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    P%  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i �%  % , O:\on_in_co\Util\dccbcdocu.i �%  �:  C:\Progress\OpenEdge\src\adm2\query.i    �%  z + C:\Progress\OpenEdge\src\adm2\delrecst.i  &  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  T&  F� ) C:\Progress\OpenEdge\gui\fnarg   �&   ( %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   �&  �   C:\Progress\OpenEdge\src\adm2\dataquery.i    �&  �Z ' %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   ,'  �< ! C:\Progress\OpenEdge\src\adm2\appserver.i    p'  �� & %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �'  I� " C:\Progress\OpenEdge\src\adm2\smart.i    �'  Ds % C:\Progress\OpenEdge\gui\fn   (  tw $ %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   H(  Q. # C:\Progress\OpenEdge\gui\set �(  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i �(  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    �(  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    ()  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  l)  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i �)  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i �)   	 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i     *  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   \*  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   �*  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �*  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i     +  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    d+  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �+  �j  C:\Progress\OpenEdge\gui\get �+  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    ,  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    H,  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �,  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �,  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �,  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   4-  �  C:\Progress\OpenEdge\src\adm2\appsprto.i x-  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �-  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    �-  ª 
 %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   0.  ��  C:\Progress\OpenEdge\src\adm2\qryprto.i  x.  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   �.  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i �.  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    $/  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   h/  ,�    O:\on_in_co\Util\dccbcdocu.w     �   �      �/  [  P     �/     N  %   �/  �   �     �/     p  .   0  �   f     0     G     (0  �   D     80     "  #   H0  �         X0     �  #   h0  �   �     x0     �  #   �0  �   �     �0     �  #   �0  �   �     �0     �  #   �0  �   �     �0     l  #   �0  �   j     �0     H  #   1  �   F     1     $  #   (1  �        81     �  -   H1  �   �     X1     �  ,   h1  k   �     x1  �  �     �1     �  +   �1  �  �     �1     |  +   �1  �  y     �1     _  +   �1  �  \     �1     B  +   �1  �  ?     2     %  +   2  �  "     (2       +   82  �       H2     �  +   X2  �  �     h2     �  +   x2  �  �     �2     �  +   �2  �  �     �2     �  +   �2  �  �     �2     w  +   �2  �  t     �2     Z  +   �2  �  W     3     =  +   3  �  :     (3        +   83  �       H3       +   X3  �        h3     �  +   x3  �  �     �3     �  +   �3  �  �     �3     �  +   �3  �  �     �3     j  #   �3  �  i     �3     G  #   �3  k  "     4        #   4  j  �     (4     �  #   84  i  �     H4     �  #   X4  _  �     h4     �  *   x4  ^  �     �4     c  *   �4  ]  b     �4     <  *   �4  \  ;     �4       *   �4  [       �4     �  *   �4  Z  �     5     �  *   5  Y  �     (5     �  *   85  X  �     H5     y  *   X5  W  x     h5     R  *   x5  V  Q     �5     +  *   �5  U  *     �5       *   �5  T       �5     �  *   �5  S  �     �5     �  *   �5  R  �     6     �  *   6  Q  �     (6     h  *   86  P  g     H6     A  *   X6  O  @     h6       *   x6  N       �6     �  *   �6  @  �     �6     �  #   �6  	  �     �6     �  )   �6  �   y     �6     W  #   �6  �   V     7     4  #   7  �   3     (7       #   87  �        H7     �  #   X7  �   �     h7     �  #   x7  �   �     �7     �  #   �7  �   8     �7     �  (   �7  g   �     �7  a   �      �7     k  '   �7  _   i      �7     G  #   8  ]   E      8     #  #   (8  I         88  �     !   H8     �  &   X8  �   �  !   h8     �  #   x8  �   �  !   �8     d  #   �8  �   b  !   �8     @  #   �8  g   &  !   �8          �8  O   �  !   �8  �   y  "   �8     w  %   9  �   G  "   9     �  $   (9  �   �  "   89     �  #   H9  �   �  "   X9     �  #   h9  �   �  "   x9     |  #   �9  �   {  "   �9     Y  #   �9  �   E  "   �9     #  #   �9  }     "   �9     �  #   �9     y  "   �9     +  !   :     �      :     }     (:     1     8:  �   (     H:  O        X:     	     h:     �     x:  �   �     �:  �   y     �:  O   k     �:     Z     �:          �:  y   �
     �:  �   �
  	   �:  G   �
     �:     �
     ;     t
     ;  c   
  	   (;  x   
     8;  M   �	     H;     �	     X;     �	     h;  a   �	     x;  �  b	     �;     C	     �;  �  	     �;  O   	     �;     �     �;     �     �;  �   �     �;     �     �;     �     <  x   �     <     �     (<     ^     8<     Z     H<     F     X<     -     h<  Q        x<     �     �<     �     �<     w     �<     ]     �<  ]   W  	   �<     M     �<       	   �<     �  
   �<     �  	   =  Z   �     =     �     (=     �     8=     �     H=     �     X=  c   a     h=     ?     x=     �      �=     �      �=     �      �=     �      �=     !       �=           