	��V��c�5  ��              c                                �L 35B4010Futf-8 MAIN D:\xpciman\progress\Menu-ERP-Progress\Menu-ERP.w,, PROCEDURE tvNodeSelect,,INPUT pcnodeKey CHARACTER PROCEDURE tvNodeEvent,,INPUT pcEvent CHARACTER,INPUT pcnodeKey CHARACTER PROCEDURE tvNodeaddOnExpand,,INPUT pcnodeKey CHARACTER PROCEDURE LogAccesos,, PROCEDURE initializeObject,, PROCEDURE exitObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE cargar-treeview,,INPUT pParent CHARACTER PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER       �1              �             o �1  �              �y              2    +   @� �  7   �� `  8   @� �  C   ԓ �  D   �� |  E   � �  F   �� $  G   ܰ �  H   x� d  I   �� P  J   ,� l	  K   �� �  L           P� L  �� T  �� �  ? �� �&  iSO8859-1                                                                           �/   . �                                      �     	             P�                �0  �    �   Z   ��  �0    41  ��  �   L1      X1                                                       PROGRESS                         �           
    
                    �              �                                                                                                     
           �          \  D#  �   <$     �  �B�_�$  
       !              4                 �   l         �       �   \  0     P  �   :O  �B�_�         �                              �   �         �       D  \  �(  �   �)  D  �  �B�_D*  
       !              �$          �%      �   <         �       �  L  �+     ,  �  ��      0,         �             l*          �*      �   �         �       �  L  |/     �/  �  �      �/         �             <,          �,      �   P  �      �  
    
                  �  �             <                                                                                          �          
  �  �      x  
    
                  d  ,  	           �                                                                                          �          
  �  �      $  
    
                    �  
           �                                                                                          �          
  T  �      �  
    
                  �  �             @                                                                                          �          
           |  
    
                  h  0             �                                                                                                    
  �  !      (  
    
                    �             �                                                                                          !          
  X  6      �  
    
                  �  �             D                                                                                          6          
  	  L      �  
    
                  l  4	             �                                                                                          L          
  �	  Z      ,	                         	  �	             �	                                                                                          Z            \
  g      �	                        �	  �
             H
                                                                                          g              u      �
  
    
                  p
  8             �
                                                                                          u          
  �  �      0  
    
                    �             �                                                                                          �          
  `  �      �  
    
                  �  �             L                                                                                          �          
    �      �                        t  <             �                                                                                          �            �  �      4                           �             �                                                                                          �            d  �      �                        �  �             P                                                                                          �                �      �                        x  `             �                                                                                          �                         INTEGRAL                         PROGRESS                         X      M"        M"                         �B�_            U"  ]                              �  �                        �  )      PROCEDIMIENTODETALLEGRUPOSAPLIC-IDVERSION                                                     �  !   �"        �"                         �B�_            �"  vh                              �  �                      \  �  s      CODMNUETIQUETATIPOPROGRAMASEGURIDAD-GRUPOSACCESO-DIRECTOICONAPLIC-IDSEGURIDAD-ATRIBUTOSPERSISTENTETECLA-ACELERADORA                                                                       	          
                                 $   g#        g#                         �B�_            p#  �N                              �  `                      �  p  .      CODCIATABLAEVENTODIAHORAUSUARIOVALORLLAVENUMID                                                                        	          �  %   ~#        ~#                         ��            ~#  t                              �                             *       �       %  B  0     P  �   :O  �B�_�         %  :O  L                      �                          �  ,   �%        �%                         �B�_            �%  15                              �  �                      �  �  #      APLIC-IDUSER-IDCODCIAADMINSEGURIDAD                                                       -   �%        �%                         �B�_            �%  ��                              �  p                      �  �  <      CODCIAUSER-IDUSER-NAMEDISABLEDCREATE-DATEDISABLED-DATECODPER                                           (          (                      	                                                                                                                                                
                                                                                       P  \  d  t  p          x             �  �  �  �  �          �             �  �  �    �                        ,  4  <  L  D          P              h  t  |  �  �          �              �  �  �  �              �              �                                  8  L  T  h              l              �  �  �  �                              �  �  �  �                              �  �                                      $  ,                                                                          Aplic-Id    X(3)    Aplicaci�n  App     Identificaci�n de la aplicaci�n CodMnu  X(6)    Opci�n  Opci�n      C�digo de la opci�n del men�    Etiqueta    X(30)   Etiqueta    Etiqueta        Etiqueta de la opci�n del men�  Tipo    X(20)   Tipo    Tipo        Tipo de opci�n de men�  Programa    X(100)  Programa    Programa        Programa a ejecutar Acceso-directo  si/no   Acceso Directo  no  Presenta icon para acceso directo   Icon    X(40)   Icon        Imagen para el acceso directo   Seguridad-Grupos    X(20)   Grupos de Acceso        Cada grupo separados por comas  Seguridad-Atributos X(8)    Atributo de Seguridad       Persistente si/no   Persistente si  Tecla-Aceleradora   X(8)    Acelerador      app-id  x(8)    app-id      �  ���������                 �%                �     i  i    	 	 	    �   �   �   �   �   �   �   �       +  =                ��                                               ��          |  �  \ �            
             
                                                                                                              
             
             
                                         
                                                                                                                \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \  l  |  �  �  �      \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \  l  |  �  �  �                                                                                                                                 	                  
                                                  �!  �!  "   "  "                         $"  ,"  4"  <"                             @"  H"  T"  \"                              `"  h"  |"  �"                             �"  �"  �"  �"                              �"  �"  �"  �"                              �"  �"  �"  �"                             �"  �"  �"  #                             #  #  #  $#                              (#  0#  8#  @#                                                                          Task-No 999999  Numero de Tarea Nro!Tarea   0   Llave-C x(8)    Llave-C     Campo-D 99/99/9999  Campo-D ?   Campo-F ->>>,>>>,>>9.9999   Campo-F 0   Campo-I ->>>,>>>,>>9    Campo-I 0   Campo-C X(8)    Campo-C     Llave-I >>>>>>>>>9  Llave-I 0   Llave-F >>>>>>>>>9  Llave-F 0   Llave-D 99/99/9999  Llave-D ?   Campo-L Si/No   Campo-L Si  �  ���������   � $�  �������������������������������� $�                                �� $�                                �� $�                                � �� B�  �     �%        �%        �%                �     i  i      i  i      i  i     	 	 		 	    3   c   ;   C   K   S   [   k   s   {                                                                                                                                  	                  
                                                  �'  �'  �'  �'  �'                         �'  �'  �'  �'                             �'  �'  �'  �'                              �'  (  (   (                             $(  ,(  <(  D(                              H(  P(  X(  `(                              d(  l(  x(  �(                             �(  �(  �(  �(                             �(  �(  �(  �(                              �(  �(  �(  �(                                                                          Task-No 999999  Numero de Tarea Nro!Tarea   0   Llave-C x(8)    Llave-C     Campo-D 99/99/9999  Campo-D ?   Campo-F ->>>,>>>,>>9.9999   Campo-F 0   Campo-I ->>>,>>>,>>9    Campo-I 0   Campo-C X(8)    Campo-C     Llave-I >>>>>>>>>9  Llave-I 0   Llave-F >>>>>>>>>9  Llave-F 0   Llave-D 99/99/9999  Llave-D ?   Campo-L Si/No   Campo-L Si  �  ���������   � $�  �������������������������������� $�                                �� $�                                �� $�                                � �� B�  �     �%        �%        �%                �     i  i      i  i      i  i     	 	 		 	    3   c   ;   C   K   S   [   k   s   {                                                                             l+  x+  �+  �+  �+                          �+  �+  �+  �+  �+                          �+  �+  �+  �+  �+                                                                      tUsuario    x(25)   tUsuario    Usuario     tNombre x(100)  tNombre Nombre y apellidos      testado x(15)   testado Estado      �  ���������   �       �%                �     i     	    �  �  �                                                                                                                                                    <.  H.  P.  h.  \.                          l.  x.  �.  �.  �.                          �.  �.  �.  �.  �.                          �.  �.  �.  �.  �.                          �.  /  /  $/  /                          (/  0/  8/  T/  @/                          X/  `/  h/  x/  p/                                                                      tCodModulo  x(15)   tCodModulo  Cod.Modulo      tNomModulo  x(50)   tNomModulo  Nombre Modulo       tDia    99/99/99    tDia    Dia de Acceso   ?   tHora   x(12)   tHora   Hora de Acceso      tUsuario    x(25)   tUsuario    Usuario     tNombre x(100)  tNombre Nombre y apellidos      testado x(15)   testado Estado      �  ���	������  �    �       �%                �     i    	 	          "  �  �  �    ��                                                                                                             �          ����                            �    ��  2                 S�    �   ��  2                 S�    �%         �%    �8    �%  ! �    �%         �%         �&  $ AX    �&  %       �%  , �4    �&  - i�    getOtherWinTargetFrame  undefined                                                               �       ��  �   l   ��    ��                  �����               4��                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       `     D          assignFocusedWidget         �      �     U      LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    i      LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    {      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          �      LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    �      LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    �      LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �      LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �      CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    "      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    2      LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    C      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 P      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    [      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    h      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    |      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    �      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    �      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    �      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
    �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER        u   ����  �             �   �           �   �            �              � ߱            Z   �����
   �p
                         u   ���� �             4  �               � ߱            Z   ����   ��
                     �    �  |  �  @  X      4   ����X      o   �       �                              �  �  NA  �  �  �  �  �     �     �    �            ,    @  `  T  
`  h  $  |    �     �      $  �  l  ���                       �     
                    � ߱        Ȃ      �  0      �      4   �����                @                      ��                                      L��                         �  �      \  l      �      4   �����      $    �  ���                       D  @         0              � ߱              
  �  �      �      4   �����      $      ���                       �  @         �              � ߱        assignPageProperty                              �  �      ��                  �  �  �              �U�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   D                            ��                  8           ��                            ����                            changePage                              0        ��                  �  �  H              自                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             0        ��                  �  �  H              �n�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `           ��                            ����                            constructObject                             \  D      ��                  �  �  t              l��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
  �             �  
             ��                �               �� 
                   
         ��                            ����                            createObjects                                  �      ��                  �  �                �K�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                                 �      ��                  �  �                0��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  0           ��                            ����                            destroyObject                               ,        ��                  �  �  D              į�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                ,        ��                  �  �  D              t��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  \           ��                            ����                            initializeObject                                \  D      ��                  �  �  t              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               l  T      ��                  �  �  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               l  T      ��                  �  �  �              8��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            notifyPage                              �  |      ��                  �  �  �              �R�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            passThrough                             �  �      ��                  �  �  �              �Z�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                 �               ��                             ��                            ����                            removePageNTarget                                 �      ��                  �  �  ,              �b�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  x             D  
             ��                  l           ��                            ����                            selectPage                              d  L      ��                  �  �  |              �ɪ                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            toolbar                             �   p       ��                  �  �  �               ު                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �            ��                            ����                            viewObject                              �!  �!      ��                  �  �  �!              l��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                �"  �"      ��                  �  �  �"              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �"           ��                            ����                            disablePagesInFolder    
      H#      �#    %      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder `#      �#      �#    :      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  �#      $      @$    N      HANDLE, getCallerWindow  $      H$      x$    a      HANDLE, getContainerMode    X$      �$      �$    q      CHARACTER,  getContainerTarget  �$      �$      �$    �      CHARACTER,  getContainerTargetEvents    �$       %      <%    �      CHARACTER,  getCurrentPage  %      H%      x%    �      INTEGER,    getDisabledAddModeTabs  X%      �%      �%     �      CHARACTER,  getDynamicSDOProcedure  �%      �%       &  !  �      CHARACTER,  getFilterSource �%      &      <&  "  �      HANDLE, getMultiInstanceActivated   &      D&      �&  #  �      LOGICAL,    getMultiInstanceSupported   `&      �&      �&  $        LOGICAL,    getNavigationSource �&      �&      '  %  /      CHARACTER,  getNavigationSourceEvents   �&      '      P'  &  C      CHARACTER,  getNavigationTarget 0'      \'      �'  '  ]      HANDLE, getOutMessageTarget p'      �'      �'  (  q      HANDLE, getPageNTarget  �'      �'      (  )  �      CHARACTER,  getPageSource   �'      (      @(  *  �      HANDLE, getPrimarySdoTarget  (      H(      |(  +  �      HANDLE, getReEnableDataLinks    \(      �(      �(  ,  �      CHARACTER,  getRunDOOptions �(      �(      �(  -  �      CHARACTER,  getRunMultiple  �(      )      4)  .  �      LOGICAL,    getSavedContainerMode   )      @)      x)  /  �      CHARACTER,  getSdoForeignFields X)      �)      �)  0         CHARACTER,  getTopOnly  �)      �)      �)  1 
       LOGICAL,    getUpdateSource �)      �)      ,*  2        CHARACTER,  getUpdateTarget *      8*      h*  3  /      CHARACTER,  getWaitForObject    H*      t*      �*  4  ?      HANDLE, getWindowTitleViewer    �*      �*      �*  5  P      HANDLE, getStatusArea   �*      �*       +  6  e      LOGICAL,    pageNTargets     +      ,+      \+  7  s      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject <+      �+      �+  8  �      LOGICAL,INPUT h HANDLE  setCallerProcedure  �+      �+      ,  9  �      LOGICAL,INPUT h HANDLE  setCallerWindow �+      (,      X,  :  �      LOGICAL,INPUT h HANDLE  setContainerMode    8,      p,      �,  ;  �      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  �,      �,       -  <  �      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �,      $-      T-  =  �      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  4-      p-      �-  >  �      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �-      �-      .  ?  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �-      0.      `.  @        LOGICAL,INPUT phObject HANDLE   setInMessageTarget  @.      �.      �.  A  $      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   �.      �.      /  B  7      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �.      @/      |/  C  Q      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource \/      �/      �/  D  k      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   �/      0      @0  E        LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget  0      d0      �0  F  �      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget x0      �0      �0  G  �      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �0      1      <1  H  �      LOGICAL,INPUT pcObject CHARACTER    setPageSource   1      `1      �1  I  �      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget p1      �1      �1  J  �      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    �1      2      D2  K  �      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget $2      p2      �2  L        LOGICAL,INPUT phObject HANDLE   setRunDOOptions �2      �2      �2  M        LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �2      3      D3  N  '      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   $3      h3      �3  O  6      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �3      �3       4  P  L      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �3      ,4      X4  Q 
 `      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource 84      x4      �4  R  k      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget �4      �4      �4  S  {      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    �4       5      T5  T  �      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    45      t5      �5  U  �      LOGICAL,INPUT phViewer HANDLE   getObjectType   �5      �5      �5  V  �      CHARACTER,  setStatusArea   �5      6      86  W  �      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �6  �6      ��                  M  N  7               �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               �7  �7      ��                  P  Q  8              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                �8  �8      ��                  S  T  9              D�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �9  �9      ��                  V  W  :               �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                                ;  �:      ��                  Y  [  ;               �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  0;           ��                            ����                            getAllFieldHandles  6      �;      �;  X  �      CHARACTER,  getAllFieldNames    �;      �;      <  Y  �      CHARACTER,  getCol  �;      <      @<  Z  �      DECIMAL,    getDefaultLayout     <      L<      �<  [  �      CHARACTER,  getDisableOnInit    `<      �<      �<  \  		      LOGICAL,    getEnabledObjFlds   �<      �<       =  ]  	      CHARACTER,  getEnabledObjHdls   �<      =      @=  ^  ,	      CHARACTER,  getHeight    =      L=      x=  _ 	 >	      DECIMAL,    getHideOnInit   X=      �=      �=  `  H	      LOGICAL,    getLayoutOptions    �=      �=      �=  a  V	      CHARACTER,  getLayoutVariable   �=       >      4>  b  g	      CHARACTER,  getObjectEnabled    >      @>      t>  c  y	      LOGICAL,    getObjectLayout T>      �>      �>  d  �	      CHARACTER,  getRow  �>      �>      �>  e  �	      DECIMAL,    getWidth    �>      �>      ?  f  �	      DECIMAL,    getResizeHorizontal �>      (?      \?  g  �	      LOGICAL,    getResizeVertical   <?      h?      �?  h  �	      LOGICAL,    setAllFieldHandles  |?      �?      �?  i  �	      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �?      �?      0@  j  �	      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    @      P@      �@  k  �	      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    d@      �@      �@  l  
      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �@      �@      ,A  m  
      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    A      LA      �A  n  $
      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout `A      �A      �A  o  5
      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal �A      �A      ,B  p  E
      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   B      XB      �B  q  Y
      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated lB      �B      �B  r  k
      LOGICAL,    getObjectSecured    �B      �B      (C  s  
      LOGICAL,    createUiEvents  C      4C      dC  t  �
      LOGICAL,    bindServer                               D  �C      ��                  =  >  D              <�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               E  �D      ��                  @  A  E              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             F  �E      ��                  C  D  $F              �ת                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                G  �F      ��                  F  G  ,G              dت                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                               H  H      ��                  I  J  8H              �۪                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             (I  I      ��                  L  M  @I              �۪                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             ,J  J      ��                  O  Q  DJ              �ܪ                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 \J  
         ��                            ����                            startServerObject                               \K  DK      ��                  S  T  tK              �d�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                `L  HL      ��                  V  X  xL              e�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �L           ��                            ����                            getAppService   DC      �L      (M  u  �
      CHARACTER,  getASBound  M      4M      `M  v 
 �
      LOGICAL,    getAsDivision   @M      lM      �M  w  �
      CHARACTER,  getASHandle |M      �M      �M  x  �
      HANDLE, getASHasStarted �M      �M      N  y  �
      LOGICAL,    getASInfo   �M      N      DN  z 	 �
      CHARACTER,  getASInitializeOnRun    $N      PN      �N  {  �
      LOGICAL,    getASUsePrompt  hN      �N      �N  |        LOGICAL,    getServerFileName   �N      �N      O  }        CHARACTER,  getServerOperatingMode  �N      O      HO  ~  "      CHARACTER,  runServerProcedure  (O      TO      �O    9      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   hO      �O      �O  �  L      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �O      $P      TP  �  Z      LOGICAL,INPUT pcDivision CHARACTER  setASHandle 4P      xP      �P  �  h      LOGICAL,INPUT phASHandle HANDLE setASInfo   �P      �P      �P  � 	 t      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �P      Q      HQ  �  ~      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  (Q      lQ      �Q  �  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   |Q      �Q      �Q  �  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �Q      R      LR  �  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             S  �R      ��                       S              �z�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  lS             8S  
             ��   �S             `S               �� 
                 �S  
         ��                            ����                            addMessage                              �T  hT      ��                  !  %  �T              `=�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �T             �T               ��   U             �T               ��                   U           ��                            ����                            adjustTabOrder                              �U  �U      ��                  '  +  V              �@�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  `V             ,V  
             �� 
  �V             TV  
             ��                  |V           ��                            ����                            applyEntry                              tW  \W      ��                  -  /  �W              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �W           ��                            ����                            changeCursor                                �X  �X      ��                  1  3  �X              �X�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �X           ��                            ����                            createControls                              �Y  �Y      ��                  5  6  �Y              xY�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �Z  �Z      ��                  8  9  �Z              �`�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �[  �[      ��                  ;  <  �[              \a�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �\  �\      ��                  >  ?  �\              b�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �]  �]      ��                  A  B  �]              q�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �^  �^      ��                  D  E  �^              �q�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �_  �_      ��                  G  H   `              �\�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �`  �`      ��                  J  O  a              �]�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  Ta              a  
             ��   |a             Ha               ��   �a             pa               ��                  �a           ��                            ����                            modifyUserLinks                             �b  |b      ��                  Q  U  �b              P��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �b             �b               ��    c             �b               �� 
                 c  
         ��                            ����                            removeAllLinks                              d  �c      ��                  W  X  (d              x§                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              e  �d      ��                  Z  ^  (e              P/�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  te             @e  
             ��   �e             he               �� 
                 �e  
         ��                            ����                            repositionObject                                �f  xf      ��                  `  c  �f              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �f             �f               ��                  �f           ��                            ����                            returnFocus                             �g  �g      ��                  e  g  �g              l�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 h  
         ��                            ����                            showMessageProcedure                                i  �h      ��                  i  l  ,i              X�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   xi             Di               ��                  li           ��                            ����                            toggleData                              dj  Lj      ��                  n  p  |j              ڧ                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �j           ��                            ����                            viewObject                              �k  tk      ��                  r  s  �k              �ڧ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  ,R      �k      (l  � 
       LOGICAL,    assignLinkProperty  l      4l      hl  �  $      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   Hl      �l      �l  �  7      CHARACTER,  getChildDataKey �l      �l      ,m  �  E      CHARACTER,  getContainerHandle  m      8m      lm  �  U      HANDLE, getContainerHidden  Lm      tm      �m  �  h      LOGICAL,    getContainerSource  �m      �m      �m  �  {      HANDLE, getContainerSourceEvents    �m      �m      ,n  �  �      CHARACTER,  getContainerType    n      8n      ln  �  �      CHARACTER,  getDataLinksEnabled Ln      xn      �n  �  �      LOGICAL,    getDataSource   �n      �n      �n  �  �      HANDLE, getDataSourceEvents �n      �n      $o  �  �      CHARACTER,  getDataSourceNames  o      0o      do  �  �      CHARACTER,  getDataTarget   Do      po      �o  �        CHARACTER,  getDataTargetEvents �o      �o      �o  �        CHARACTER,  getDBAware  �o      �o      p  � 
 #      LOGICAL,    getDesignDataObject �o      $p      Xp  �  .      CHARACTER,  getDynamicObject    8p      dp      �p  �  B      LOGICAL,    getInstanceProperties   xp      �p      �p  �  S      CHARACTER,  getLogicalObjectName    �p      �p       q  �  i      CHARACTER,  getLogicalVersion    q      ,q      `q  �  ~      CHARACTER,  getObjectHidden @q      lq      �q  �  �      LOGICAL,    getObjectInitialized    |q      �q      �q  �  �      LOGICAL,    getObjectName   �q      �q      r  �  �      CHARACTER,  getObjectPage   �q      (r      Xr  �  �      INTEGER,    getObjectParent 8r      dr      �r  �  �      HANDLE, getObjectVersion    tr      �r      �r  �  �      CHARACTER,  getObjectVersionNumber  �r      �r      s  �  �      CHARACTER,  getParentDataKey    �r       s      Ts  �  	      CHARACTER,  getPassThroughLinks 4s      `s      �s  �        CHARACTER,  getPhysicalObjectName   ts      �s      �s  �  .      CHARACTER,  getPhysicalVersion  �s      �s      t  �  D      CHARACTER,  getPropertyDialog   �s      $t      Xt  �  W      CHARACTER,  getQueryObject  8t      dt      �t  �  i      LOGICAL,    getRunAttribute tt      �t      �t  �  x      CHARACTER,  getSupportedLinks   �t      �t      u  �  �      CHARACTER,  getTranslatableProperties   �t      u      Xu  �  �      CHARACTER,  getUIBMode  8u      du      �u  � 
 �      CHARACTER,  getUserProperty pu      �u      �u  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �u      �u      ,v  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles v      Tv      �v  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    `v      �v      �v  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry �v      w      <w  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   w      �w      �w  �  	      CHARACTER,INPUT piMessage INTEGER   propertyType    �w      �w      ,x  �        CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  x      Tx      �x  �  $      CHARACTER,  setChildDataKey dx      �x      �x  �  3      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  �x      �x      y  �  C      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  �x      <y      py  �  V      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    Py      �y      �y  �  i      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �y      �y      $z  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   z      Lz      |z  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents \z      �z      �z  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �z      �z      ,{  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   {      T{      �{  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents d{      �{      �{  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  �{       |      ,|  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject |      L|      �|  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    `|      �|      �|  �        LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   �|      �|      0}  �        LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    }      T}      �}  �  3      LOGICAL,INPUT c CHARACTER   setLogicalVersion   l}      �}      �}  �  H      LOGICAL,INPUT cVersion CHARACTER    setObjectName   �}       ~      0~  �  Z      LOGICAL,INPUT pcName CHARACTER  setObjectParent ~      P~      �~  �  h      LOGICAL,INPUT phParent HANDLE   setObjectVersion    `~      �~      �~  �  x      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    �~      �~      0  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks       X      �  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   l      �      �  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �      �      8�  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      \�      ��  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   l�      ��      �  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   Ȁ      �      H�  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  (�      l�      ��  � 
       LOGICAL,INPUT pcMode CHARACTER  setUserProperty x�      ��      �  �        LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ȁ      (�      T�  �  .      LOGICAL,INPUT pcMessage CHARACTER   Signature   4�      x�      ��  � 	 :      CHARACTER,INPUT pcName CHARACTER    ��    �  �  `�            4   ����                p�                      ��                  �  �                  �ϧ                       �  �        �  ��  �            4   ����                �                      ��                  �  �                  0Ч                       �  ��  �    �  4�  ��      0      4   ����0                ��                      ��                  �  �                  �Ч                       �  D�         �                                       
                    � ߱        D�  $  �  �  ���                           $  �  p�  ���                       P                         � ߱        ��    �  ��  4�      `      4   ����`                D�                      ��                  �  �	                  �                       �  ȅ  x�  o   �      ,                                 І  $   �  ��  ���                       �  @         �              � ߱        �  �   �  �      ��  �   �  h      �  �   �  �       �  �   �  P      4�  �   �  �      H�  �   �  8      \�  �   �  �      p�  �   �  �      ��  �   �  d	      ��  �   �  �	      ��  �   �  T
      ��  �   �  �
      ԇ  �   �  L      �  �   �  �      ��  �   �        �  �   �  x      $�  �   �  �      8�  �   �  (      L�  �   �  d      `�  �   �  �      t�  �   �  L      ��  �   �  �      ��  �   �  D      ��  �   �  �      Ĉ  �   �  4      ؈  �   �  �      �  �   �         �  �   �  X      �  �   �  �      (�  �   �        <�  �   �  |      P�  �   �  �      d�  �   �  �      x�  �    	  0      ��  �   	  l      ��  �   	  �      ��  �   	  $      ȉ  �   	  `      ܉  �   	  �      ��  �   	  �      �  �   		        �  �   
	  P      ,�  �   	  �      @�  �   	  �          �   	                        l�          ؊  ��      ��                  �	  �	  ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��      t     
                �                                               � ߱        ��  $ �	  �  ���                           O   �	  ��  ��  @               �          �  ��    �                                             ��                            ����                                �5      T�      ��     6     �                      V �  �                     h�    �	  Č  @�      L      4   ����L                P�                      ��                  �	  |
                  �ߧ                       �	  Ԍ  d�  �   �	  �      x�  �   �	         ��  �   �	  �      ��  �   �	        ��  �   �	  �      ȍ  �   �	        ܍  �   �	  �      ��  �    
         �  �   
  |      �  �   
  �      ,�  �   
  l      @�  �   
  �      T�  �   
  d          �   
  �      @�    �
  ��   �      P      4   ����P                �                      ��                  �
                    ��                       �
  ��  $�  �   �
  �      8�  �   �
  $       L�  �   �
  �       `�  �   �
  !      t�  �   �
  �!      ��  �   �
  �!      ��  �   �
  x"      ��  �   �
  �"      ď  �   �
  `#      ؏  �   �
  �#      �  �   �
  P$       �  �   �
  �$      �  �   �
  8%      (�  �   �
  �%      <�  �   �
  0&      P�  �   �
  �&      d�  �   �
  ('      x�  �   �
  �'      ��  �   �
   (      ��  �   �
  �(      ��  �   �
  )      Ȑ  �   �
  �)      ܐ  �   �
  *      �  �   �
  �*      �  �   �
  +      �  �   �
  �+      ,�  �   �
   ,          �   �
  |,      \�    "  \�  ؑ      �,      4   �����,                �                      ��                  #  �                  �"�                       #  l�  ��  �   &  D-      �  �   '  �-      $�  �   (  <.      8�  �   )  �.      L�  �   +  $/      `�  �   ,  �/      t�  �   .  0      ��  �   /  H0      ��  �   0  �0      ��  �   1  �0      Ē  �   2  41      ؒ  �   3  �1      �  �   4  2       �  �   5  �2      �  �   7  3      (�  �   8  �3      <�  �   9  �3      P�  �   :  p4      d�  �   ;  �4      x�  �   <  (5      ��  �   >  �5      ��  �   ?  6      ��  �   @  �6      ȓ  �   A  �6      ܓ  �   B  �6      �  �   C  x7      �  �   D  �7      �  �   E  �7      ,�  �   F  ,8      @�  �   G  h8      T�  �   H  �8      h�  �   I  �8      |�  �   J  9      ��  �   L  �9      ��  �   M  �9      ��  �   N  :      ̔  �   O  D:      ��  �   P  �:      ��  �   Q  �:      �  �   R  �:      �  �   S  4;      0�  �   T  �;      D�  �   U  <      X�  �   V  �<      l�  �   W  =      ��  �   X  �=      ��  �   Y  �=      ��  �   Z  x>      ��  �   [  �>      Е  �   \  p?      �  �   ]  �?      ��  �   ^  (@      �  �   _  �@       �  �   `  �@      4�  �   a  A      H�  �   b  XA          �   c  �A      ��  $  �  ��  ���                       4B     
                    � ߱        L�      Ж  ��      @B      4   ����@B      /     �     �                          3   ����PB            <�                      3   ����pB  ��    #  h�  �  Л  �B      4   �����B  	              ��                      ��             	     $  �                  � �                       $  x�  �  �   (  �B      `�  $  )  4�  ���                       C     
                    � ߱        t�  �   *  8C      ̘  $   ,  ��  ���                       `C  @         LC              � ߱        ��  $  /  ��  ���                       �C                         � ߱        (D     
                �D                     �E  @        
 �E              � ߱        �  V   9  $�  ���                         F                     4F                     pF                         � ߱        ��  $  U  ��  ���                       0G     
                �G                     �H  @        
 �H              � ߱        8�  V   g  D�  ���                        I     
                �I                     �J  @        
 �J              � ߱            V   �  Ԛ  ���                        
              ��                      ��             
     �  G                  X�                       �  d�  �J     
                \K                     �L  @        
 lL          M  @        
 �L          pM  @        
 0M          �M  @        
 �M              � ߱            V   �  ��  ���                        adm-clone-props L�  Ĝ              �     7     `                          \  �                     start-super-proc    Ԝ  0�  �           �     8                                  �                     8�    _  ��  ̝      \Q      4   ����\Q      /   `  ��     �                          3   ����lQ            (�                      3   �����Q  ��  $  z  d�  ���                       �Q                         � ߱        L�    �  ��  (�  ȟ  �Q      4   �����Q                ��                      ��                  �  �                  ��                       �  ��  �Q                     �Q                     R                         � ߱            $  �  8�  ���                             �  �   �      R      4   ����R  <R                         � ߱            $  �  ��  ���                       H�    �  h�  x�  Р  PR      4   ����PR      $  �  ��  ���                       pR                         � ߱            �   �  �R      �R     
                @S                     �T  @        
 PT              � ߱        t�  V   �  �  ���                        ��  �   �  �T       �      ��  ��      �T      4   �����T      /   �  �     �                          3   �����T            �                      3   ����U  ܢ  $  �  L�  ���                       (U                         � ߱        TU     
                �U                      W  @        
 �V              � ߱        �  V   �  x�  ���                        �    	  $�  ��      ,W      4   ����,W                ��                      ��                  
                    t��                       
  4�      g     ȣ         ����                           ��          `�  H�      ��                        x�              ���                    O   ����    e�          O   ����    R�          O   ����    ��          /    ��     ̤  TW                      3   ����<W  ��     
   �                      3   ����`W         
   �                      3   ����hW    ��                              ��        �                  ����                                        ܣ              9      ,�                      g                               �  g      �          ��	��                           Ȧ          ��  ��      ��                      ��              |��                    O   ����    e�          O   ����    R�          O   ����    ��          /    ��     �  �W                      3   ����pW            $�                      3   �����W    ��                              ��        �                  ����                                        �              :      4�                      g                               ��  g     �          ��	��                           Ш          ��  ��      ��                      ��              �Ǫ                    O   ����    e�          O   ����    R�          O   ����    ��          /    ��     �  �W                      3   �����W            ,�                      3   �����W    ��                              ��        �                  ����                                        �              ;      <�                      g                               X�    ,  �  ��      �W      4   �����W                ��                      ��                  -  L                  dȪ                       -  $�  �  /   .  ̪     ܪ                          3   ���� X            ��                      3   ���� X  �  /  0  8�     H�  \X                      3   ����<X  x�     
   h�                      3   ����dX  ��        ��                      3   ����lX  ث        ȫ                      3   �����X            ��                      3   �����X  0�    8  $�  4�      �X      4   �����X      /  >  `�     p�  PY                      3   ����0Y  ��     
   ��                      3   ����XY  Ь        ��                      3   ����`Y   �        �                      3   ����tY             �                      3   �����Y        D  L�  \�      �Y      4   �����Y      /  G  ��     ��  Z                      3   �����Y  ȭ     
   ��                      3   ����Z  ��        �                      3   ����Z  (�        �                      3   ����0Z            H�                      3   ����LZ  �    P  t�  �      pZ      4   ����pZ                 �                      ��                  Q  T                  �g�                       Q  ��      g   R  �         ����        �Z                  �          ��  ��      ��                  S      ȯ              h�                    O   ����    e�          O   ����    R�          O   ����    ��          /  S  �     �  �Z                      3   �����Z  L�     
   <�                      3   �����Z         
   l�                      3   �����Z    ��                            ����                                        ,�              <      |�                      g                               ��     X  �Z                                     �Z     
                P[                     �\  @        
 `\              � ߱        @�  V   �  L�  ���                        �\     
                0]                     �^  @        
 @^              � ߱        l�  V   �  ܱ  ���                        �    *  ��  ��      �^      4   �����^      $   +  Ĳ  ���                       �^  @         �^              � ߱        Ĵ  g   U  �         ��h�        _  ��h�        _                  �          ��  ��      ��                  V  [  ̳              ���                    O   ����    e�          O   ����    R�          O   ����    ��            Z   �  �       _      4   ���� _      O  Z  ������  4_    ��                            ����                                        0�              =      (�                      g                               p�  g   b  ܴ         �6�         H_                  ��          t�  \�      ��                  c  h  ��              X,�                    O   ����    e�          O   ����    R�          O   ����    ��      ��    f  T_  }          O  g  ������  h_    ��                            ����                                        �              >      Ե                      g                               p�  g   p  ��         �"�                           P�           �  �      ��                 q  �  8�              �,�                    O   ����    e�          O   ����    R�          O   ����    ��      ̷  �   s           ܷ      p�  �      @�  (�      ��                  t  y  X�              ps�                �     t  `�      �  X�       ��                            7   ����          ��                     �            ��                  6   t        ̸   ��                    �            ��                                                                �  �                                   @            �   ��        O   ����  e�          O   ����  R�          O   ����  ��      �  9   u     |_                     �_                     �_                         � ߱            $  v  ��  ���                       L�  /  }  <�                               3   �����_  ��  $  �  x�  ���                       `                         � ߱        ��  $  �  к  ���                       $`                         � ߱        T�  $  �  (�  ���                       P`                         � ߱        ��  $  �  ��  ���                       |`                         � ߱        �  $  �  ػ  ���                       �`                         � ߱        (�  /  �  0�     @�  a                      3   �����`  p�     
   `�                      3   ����a  ��        ��                      3   ����$a            ��  м                  3   ����0a      $   �  ��  ���                                                   � ߱        L�  /  �  T�     d�  \a                      3   ����<a  ��     
   ��                      3   ����ha  Ľ        ��                      3   ����|a            �  ��                  3   �����a      $   �   �  ���                                                   � ߱        \�  �   �  �a      	   �  ��                                      ��  3   �����a  ��  3   �����a      3   �����a                (�                                               |�          \�  l�   @ ,�                
                                                 0              0   �      ��                              ��        �                   ��                            ����                                =   y     D�          ��  ��  ��    ?     ��                      g   ��                          H�  g   �  ��         �"��                           P�           �  �      ��                  �  �  8�              �9�                    O   ����    e�          O   ����    R�          O   ����    ��          /   �  |�                                 3   �����a    ��                              ��        �                  ����                                        ��              @      ��                      g                                     �  d�  ��      �a      4   �����a                T�                      ��                  �  �                  ���                       �  t�  �a  @                      b  @         b          Hb  @         4b              � ߱        ��  $   �  ��  ���                       |�  g   �  ��         �n �      }                      `�          0�  �      ��                  �  �  H�              0��                    O   ����    e�          O   ����    R�          O   ����    ��      ��  /  �  ��                                 3   ����Tb        �  ��  ��      pb      4   ����pb      O  �  ������  �b    ��                            ����                                        ��              A      ��                      g                               P�  g   �  ��         �!��         �b                  ��          ,�  �      ��                  �  �  D�              ܧ�                    O   ����    e�          O   ����    R�          O   ����    ��      �b  @                         � ߱            $  �  \�  ���                         ��                            ����                                        ��              B      ��                      g                               ��  /   �  |�                                 3   �����b        �  ��  $�      �b      4   �����b                ��                      ��                  �  �                  ���                       �  ��                ��          ��  ��      ��                 �  �                  �Z�                       �  4�      O   �    ��          O   �    ��      �  /   �  �                                 3   ���� c        �  8�  H�       c      4   ���� c      k   �  d�              }       n        �   adm-create-objects  �  |�              �     C     4                          0  1"                     cargar-treeview ��  ��  �       `  �  "  D     �                          �  #                     disable_UI  ��  X�                      E      <                              #  
                   enable_UI   d�  ��                      F      �             X              #  	                   exitObject  ��  (�                      G      �                               '#  
                   initializeObject    4�  ��                      H      \                              2#                     LogAccesos  ��   �          �  0  # & I     �                          �  �#  
                   tvNodeaddOnExpand   �  h�  �           �    ' J                                 �#                     tvNodeEvent |�  ��  �                ( K     	                          	  s%                     tvNodeSelect    ��  @�  �       �  �  + ) L     X             p          P  �%                      � ���� �   ��      ���  �                �  8   ����-   �  8   ����-   D�  - 	 $�  8   ����,   4�  8   ����,   L�  8   ����%   \�  8   ����%   ��  %  l�  8   ����$   |�  8   ����$   ��  8   ����   ��  8   ����   ��  8   ����!   ��  8   ����!   ��  8   ����   ��  8   ����   ��  8   ����    �  8   ����    T�     �  8   ����   $�  8   ����   4�  8   ����   D�  8   ����       8   ����       8   ����             l�  x�      toggleData  ,INPUT plEnabled LOGICAL    \�  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��   �  �      returnFocus ,INPUT hTarget HANDLE   ��  4�  H�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    $�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE t�  ��  ��      removeAllLinks  ,   ��  �  �      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  p�  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    `�  ��  �      hideObject  ,   ��  �  4�      editInstanceProperties  ,   �  H�  X�      displayLinks    ,   8�  l�  |�      createControls  ,   \�  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  �  �      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  l�  x�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER \�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  ,�  <�      unbindServer    ,INPUT pcMode CHARACTER �  d�  x�      startServerObject   ,   T�  ��  ��      runServerObject ,INPUT phAppService HANDLE  |�  ��  ��      restartServerObject ,   ��  ��  �      initializeServerObject  ,   ��  �  0�      disconnectObject    ,   �  D�  X�      destroyServerObject ,   4�  l�  x�      bindServer  ,   \�  ��  ��      processAction   ,INPUT pcAction CHARACTER   |�  ��  ��      enableObject    ,   ��  ��  ��      disableObject   ,   ��  �  �      applyLayout ,    �  0�  <�      viewPage    ,INPUT piPageNum INTEGER     �  h�  t�      viewObject  ,   X�  ��  ��      toolbar ,INPUT pcValue CHARACTER    x�  ��  ��      selectPage  ,INPUT piPageNum INTEGER    ��  ��  �      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER ��  D�  P�      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  4�  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��  ��  ��      initPages   ,INPUT pcPageList CHARACTER ��  �   �      initializeVisualContainer   ,   ��  4�  @�      hidePage    ,INPUT piPageNum INTEGER    $�  l�  |�      destroyObject   ,   \�  ��  ��      deletePage  ,INPUT piPageNum INTEGER    ��  ��  ��      createObjects   ,   ��  ��  ��      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  p�  |�      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  `�  ��  ��      changePage  ,   ��  ��  ��      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 �%     adecomm/as-utils.w 
"   
   �    }        �
"     
   %       	       %              %       	       %       	       %              4         %              4       %              4       %              4         %                  �     }        �G�    �G%              �      %         %       %        %       %        %       %               %               %               %              %              %              %               %              
�        
"   
 �
�    
"   
 �
"   
 �    �        �     �        �    
"   
   �        $         �     }        �%              
"   
 �
"   
 �    �        t     �        �    
"   
   �        �         �     }        �%              � 
"    
 �%              � �  �         �      T     @     $              
�    � D   �     
"   
 K� D   �     
�             �G                      
�            � F   K
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 K�           �    1� V  
 K� a   �%               o%   o           � f    K
"   
 K�           \    1� g   K� a   �%               o%   o           � u   K
"   
 K�           �    1� |  
 K� a   �%               o%   o           � �   K
"   
 K�           D    1� �   K� a   �%               o%   o           � �   K
"   
 K�           �    1� �   K� a   �%               o%   o           � �   K
"   
 K�           ,    1� �   K� �   �%               o%   o           %               
"   
 ��          �    1� �   �� �     
"   
 K�           �    1� �   K� a   �%               o%   o           �   e K
"   
 K�           X	    1� r   K� a   �%               o%   o           � �  [ K
"   
 K�           �	    1� �   K� �   �%               o%   o           %               
"   
 K�           H
    1� �   K� �   �%               o%   o           %               
"   
 K�           �
    1� �   K� �   �%               o%   o           %              
"   
 ��          @    1�    �� �     
"   
 K�           |    1�   
 K� �   �%               o%   o           %               
"   
 K�           �    1� &   K� a   �%               o%   o           � f    K
"   
 ��          l    1� .   �� �     
"   
 K�           �    1� >   K� a   �%               o%   o           � T  t K
"   
 ��              1� �  
 �� �     
"   
 K�           X    1� �   K� a   �%               o%   o           � �  � K
"   
 K�           �    1� r   K� a   �%               o%   o           � f    K
"   
 K�           @    1� �  
 K� �   �%               o%   o           %               
"   
 I�           �    1� �   I� �   �%               o%   o           %               
"   
 I�           8    1� �   I� a   �%               o%   o           � f    I
"   
 I�           �    1� �   I� a   �%               o%   o           o%   o           
"   
 I�           (    1� �  
 I� a   �%               o%   o           � f    I
"   
 I�           �    1� �   I� �  	 �%               o%   o           � �  / I
"   
 ��              1�    �� �  	   
"   
 I�           L    1� )   I� �  	 �o%   o           o%   o           � f    I
"   
 ��          �    1� <   �� �  	   
"   
 I�           �    1� K   I� �  	 �o%   o           o%   o           � f    I
"   
 ��          p    1� [   �� �     
"   
 ��          �    1� i   �� �  	   
"   
 ��          �    1� v   �� �  	   
"   
 ��          $    1� �   �� �  	   
"   
 I�           `    1� �   I� �   �o%   o           o%   o           %              
"   
 ��          �    1� �   �� �  	   
"   
 ��              1� �  
 �� �     
"   
 ��          T    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��              1� �   �� �  	   
"   
 ��          D    1� 	  	 �� �  	   
"   
 ��          �    1�    �� �  	   
"   
 ��          �    1� &   �� �  	   
"   
 I�           �    1� =   I� a   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 K
"   
   
"   
 �(�  L ( l       �        �    �� I   � P   �        �    �@    
� @  , 
�       �    �� R     p�               �L
�    %              � 8      �    � $         � Y          
�    � s     
"   
 �� @  , 
�       �    �� |  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 K�           �    1� v  
 K� a   �%               o%   o           � f    K
"   
 K�               1� �  
 K� a   �%               o%   o           o%   o           
"   
 K�           �    1� �   K� �   �%               o%   o           o%   o           
"   
 K�               1� �   K� �   �%               o%   o           %               
"   
 K�           �    1� �   K� �   �%               o%   o           %               
"   
 K�               1� �   K� a   �%               o%   o           � f    K
"   
 K�           x    1� �   K� �   �%               o%   o           %              
"   
 K�           �    1� �   K� �   �%               o%   o           o%   o           
"   
 K�           p    1� �   K� a   �%               o%   o           o%   o           
"   
 K�           �    1� �  	 K� a   �%               o%   o           � f    K
"   
 K�           `    1� �   K� a   �%               o%   o           o%   o           
"   
 K�           �    1�    K� a   �%               o%   o           o%   o           
"   
 K�           X    1�    K� �   �%               o%   o           %               
"   
 K�           �    1� !   K� �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 I�           �    1� -   I� �  	 �%               o%   o           � f    I
"   
 I�                1� :   I� �  	 �%               o%   o           � f    I
"   
 I�           �     1� H   I� �   �%               o%   o           %               
"   
 I�           !    1� V   I� �  	 �%               o%   o           � f    I
"   
 I�           |!    1� e   I� �  	 �%               o%   o           � f    I
"   
 I�           �!    1� s   I� �   �%               o%   o           %               
"   
 I�           l"    1� �   I� �  	 �%               o%   o           � f    I
"   
 I�           �"    1� �   I� �  	 �%               o%   o           � f    I
"   
 I�           T#    1� �   I� �  	 �%               o%   o           � f    I
"   
 I�           �#    1� �   I� �  	 �%               o%   o           o%   o           
"   
 I�           D$    1� �   I� �  	 �%               o%   o           � f    I
"   
 I�           �$    1� �   I� �  	 �%               o%   o           � f    I
"   
 I�           ,%    1� �  	 I� �   �%               o%   o           %               
"   
 I�           �%    1� �   I� �   �%               o%   o           %               
"   
 I�           $&    1� �   I� �   �%               o%   o           o%   o           
"   
 I�           �&    1� �   I� �   �%               o%   o           o%   o           
"   
 I�           '    1�    I� �   �%               o%   o           %               
"   
 I�           �'    1�    I� �   �%               o%   o           %               
"   
 I�           (    1� +   I� �   �%               o%   o           %               
"   
 I�           �(    1� @   I� L   �%               o%   o           %       
       
"   
 I�           )    1� T   I� L   �%               o%   o           o%   o           
"   
 I�           �)    1� `   I� L   �%               o%   o           %              
"   
 I�           *    1� l   I� L   �%               o%   o           o%   o           
"   
 I�           �*    1� x   I� L   �%               o%   o           %              
"   
 I�           �*    1� �   I� L   �%               o%   o           o%   o           
"   
 I�           x+    1� �   I� L   �%               o%   o           %              
"   
 I�           �+    1� �   I� L   �%               o%   o           o%   o           
"   
 I�           p,    1� �   I� �  	 �%               o%   o           � f    IP �L 
�H T   %              �     }        �GG %              
"   
 K�           8-    1� �   K� �   �%               o%   o           %               
"   
 K�           �-    1� �   K� �   �%               o%   o           o%   o           
"   
 K�           0.    1� �   K� a   �%               o%   o           � f    K
"   
 K�           �.    1� �   K� a   �%               o%   o           � �  - K
"   
 K�           /    1�     K� a   �%               o%   o           � f    K
"   
 K�           �/    1� 7   K� a   �%               o%   o           � T   K
"   
 ��           0    1� r   �� �     
"   
 K�           <0    1� �   K� a   �%               o%   o           � f    K
"   
 ��          �0    1� �  
 �� �     
"   
 ��          �0    1� �   �� �     
"   
 K�           (1    1� �   K� �  	 �%               o%   o           � f    K
"   
 K�           �1    1� �   K� a   �%               o%   o           � f    K
"   
 K�           2    1� �   K� �   �%               o%   o           o%   o           
"   
 K�           �2    1� �   K� a   �%               o%   o           � �  ! K
"   
 K�            3    1�    K� a   �%               o%   o           � f    K
"   
 K�           t3    1�    K� a   �%               o%   o           � #   K
"   
 K�           �3    1� 2  	 K� �   �%               o%   o           o%   o           
"   
 K�           d4    1� <   K� �   �%               o%   o           %               
"   
 ��          �4    1� H   �� �     
"   
 K�           5    1� V   K� a   �%               o%   o           � j   K
"   
 K�           �5    1� y   K� �  	 �%               o%   o           � f    K
"   
 K�           6    1� �   K� �  	 �%               o%   o           � f    K
"   
 ��          x6    1� �   �� �     
"   
 ��          �6    1� �   �� �  	   
"   
 I�           �6    1� �   I� �   �o%   o           o%   o           %               
"   
 ��          l7    1� �   �� �     
"   
 ��          �7    1� �   �� �  	   
"   
 ��          �7    1� �   �� �  	   
"   
 ��           8    1� 
   �� �  	   
"   
 ��          \8    1�    �� �  	   
"   
 ��          �8    1� ,   �� �  	   
"   
 ��          �8    1� =   �� �     
"   
 I�           9    1� N   I� a   �%               o%   o           � e  4 I
"   
 ��          �9    1� �   �� �     
"   
 ��          �9    1� �   �� �     
"   
 ��          �9    1� �   �� �     
"   
 ��          8:    1� �   �� �  	   
"   
 ��          t:    1� �   �� �  	   
"   
 ��          �:    1� �   �� �  	   
"   
 ��          �:    1� �   �� �     
"   
 I�           (;    1� 	   I� �  	 �%               o%   o           � f    I
"   
 I�           �;    1�    I� �  	 �%               o%   o           � f    I
"   
 I�           <    1� #   I� �  	 �%               o%   o           � f    I
"   
 I�           �<    1� 8   I� �  	 �%               o%   o           � f    I
"   
 I�           �<    1� M   I� �   �%               o%   o           %               
"   
 I�           t=    1� [   I� �   �%               o%   o           o%   o           
"   
 I�           �=    1� m   I� �   �%               o%   o           %               
"   
 I�           l>    1� }   I� �   �%               o%   o           %               
"   
 I�           �>    1� �   I� �   �%               o%   o           o%   o           
"   
 I�           d?    1� �   I� �   �%               o%   o           %               
"   
 ��          �?    1� �   �� �  	   
"   
 I�           @    1� �   I� �   �%               o%   o           %              
"   
 ��          �@    1� �   �� �  	   
"   
 ��          �@    1� �   �� �  	   
"   
 ��          A    1� �  
 �� �  	   
"   
 I�           LA    1� �   I� �  	 �%               o%   o           � M   I
"   
 I�           �A    1� 	   I� �  	 �%               o%   o           � f    I
"   
    "    �%     start-super-proc n�%     adm2/smart.p ��P �L 
�H T   %              �     }        �GG %              
"   
   �       �B    6� I     
"   
   
�        C    8
"   
   �        ,C    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        tD    �� I   � P   �        �D    �@    
� @  , 
�       �D    �� R   �p�               �L
�    %              � 8      �D    � $         � Y          
�    � s   �
"   
 �p� @  , 
�       �E    �� �   �p�               �L"    , �   � F   I� H   ��     }        �A      |    "      � F   I%              (<   \ (    |    �     }        �A� J   �A"    I    "    �"    I  < "    �"    I(    |    �     }        �A� J   �A"    I
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        |G    �� I   � P   �        �G    �@    
� @  , 
�       �G    �� R   �p�               �L
�    %              � 8      �G    � $         � Y          
�    � s   �
"   
 �p� @  , 
�       �H    �� V  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        TI    �� I   � P   �        `I    �@    
� @  , 
�       lI    �� R   �p�               �L
�    %              � 8      xI    � $         � Y          
�    � s   �
"   
 �p� @  , 
�       �J    �� �   �p�               �L
"   
 , 
�H T   %              �     }        �GG %              
"   
   
"   
 K
"   
   
"   
   (�  L ( l       �        ,K    �� I   � P   �        8K    �@    
� @  , 
�       DK    �� R     p�               �L
�    %              � 8      PK    � $         � Y          
�    � s     
"   
 �p� @  , 
�       `L    �� |  
 �p�               �L%     SmartWindow 
"   
   p� @  , 
�       �L    �� �     p�               �L%      WINDOW  
"   
  p� @  , 
�       $M    �� K    p�               �L%               
"   
  p� @  , 
�       �M    �� )    p�               �L(        � f      � f      � f      �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
 �    �        dN    �� I   �
"   
   � 8      �N    � $         � Y          
�    � s   �
"   
   �        O    �
"   
   �       (O    /
"   
   
"   
   �       TO    6� I     
"   
   
�        �O    8
"   
   �        �O    �
"   
   �       �O    �
"   
   p�    � s   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �P    �A"    �A
"   
   
�        �P    �@ � 
"   
 �"      �       }        �
"   
 �%              %                "    �%     start-super-proc m�%     adm2/appserver.p ���    � �     
�    �     }        �%               %      Server  - �     }        �    "    �� f    �%                   "    �� f    �%      NONE    p�,  8         $     "    K        �    �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        S    �� I   � P   �        S    �@    
� @  , 
�       (S    �� R   �p�               �L
�    %              � 8      4S    � $         � Y          
�    � s   �
"   
 �p� @  , 
�       DT    �� �   �p�               �L"    , p�,  8         $     "    K        �    �
�     "    �%     start-super-proc l�%     adm2/visual.p ��   � D     � @     � B  N   
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �U    �� I   � P   �        �U    �@    
� @  , 
�       �U    �� R   �p�               �L
�    %              � 8      �U    � $         � Y          
�    � s   �
"   
 �p� @  , 
�       �V    �� �   �p�               �L"    , � 
" 	   
 �%     contextHelp 
" 	   
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP ��%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc k�%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents K%      initializeDataObjects K0 0   A    �    � �   I
�    � �   �A    �    � �     
�    � �   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents I%     buildDataRequest ent0 A    �    � �   �
�    �     �%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
" 	   
 �
"   
 �%     contextHelp 
" 	   
   
�    
�    %               
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 K(�  L ( l       �         [    �� I   � P   �        ,[    �@    
� @  , 
�       8[    �� R   �p�               �L
�    %              � 8      D[    � $         � Y   �     
�    � s   �
"   
 �p� @  , 
�       T\    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �         ]    �� I   � P   �        ]    �@    
� @  , 
�       ]    �� R   �p�               �L
�    %              � 8      $]    � $         � Y   �     
�    � s   �
"   
 �p� @  , 
�       4^    �� M   �p�               �L%              (        �     }        �G�    �G� 
"   
 �
"   
   �        �^    �%              
"   
 �
"   
 ��     }        �%               
"   
 �%      CLOSE   %               4         %              4         %              4         %              %     lib\Tools-to-excel     (�    � e      �   � y      � {      "      �   � y      � }      "      �   �       � �      "       (         �     }        �"    �� �      %     pi-crea-archivo-csv 
"   
   
�     
        �G"      "      %     pi-crea-archivo-xls 
"   
   
�     
        �G"      "      
"   
   � �      %      
       "      % 
    LogAccesos � 
"   
 �
"   
 �
"   
 ��         b    %%              
�     }        �
"   
   %     destroyObject       �     }        �    �  � �   	   %               
"   
 �
�    %     createObjects    �     }        �%     initializeObject k� �     }        ��     "      %               %     constructObject %     pure4gltv.w 
�             �G%\SL  wineModeAutomaticwindowsSkinAutomaticpicCacheCoef1labCacheCoef1tvIterationHeight17TreeStyle3FocSelNodeBgColor1UnfSelNodeBgColor8tvnodeDefaultFont1FocSelNodeFgColor15UnfSelNodeFgColor0resizeVertical?resizeHorizontal?DragSourcenoneautoSortnoMSkeyScrollForcePaintyesHideOnInitnoDisableOnInitnoObjectLayout 
"   
   %     repositionObject k�
"   
   %         %            %     resizeObject    
"   
   %         %        	   %      addLink 
"   
   %     tvNodeEvent 
�    %     adjustTabOrder  
"   
   
�            �G%      AFTER       "    �� L"    �� L"      %      addNode 
"   
   "     �� L"    �"     �� b"   �� t"   �"  	         "  	    "      "      �            B"      �             B"      �            B" "     " "   �&    &     4    z     "  	          z     " "     %              � �"     � L"      � L"          "      � �      � L"          "      � �      � �"         "      � �      "          "      � �      � t"         "      � �      � �"         "      � �      "      %      addNode 
"   
   "  	  �"    �" "   �" "   �" "   �p�    � �"  	 �
"   
 �(        �     }        �G�    �G� 
"   
 �
"   
   �     }        �
�    
"   
 �"    K"    �"      "  
    "  	    "      "      
"   
 �4       %              4       %              
"   
   %      CLOSE   %               %      SUPER   %     cargar-treeview � L"      �            B�       
     B    +  %              �    }        �� _#     %              " #     " #     " #     &    &    &    &    &    &    T    0        %              %                  " $     &    %              " #     � L"      " $     " $     " $     � L"      � L"      " $   �&    &    * %   " %     ((       " %     %              � �#     � �#     " #   �&    &    *     "       %     lib\Tools-to-excel      � �#         (�    � e    ��   � y      � {      " #     �   � y      � }      " #     �   �       � �      " #      (         �     }        �" #   �� �      %     pi-crea-archivo-csv 
" &  
   
�     
        �G" &     " &     %     pi-crea-archivo-xls 
" &  
   
�     
        �G" &     " &     
" &  
   �    }        �� L"      � �      %      
       " &     %     cargar-treeview " '     %     tvNodeSelect    " '     " (     � t"     %     tvNodeaddOnExpand �" (     � �#     %     tvNodeSelect    " (     � �#  
   %     tvNodeCreatePopup �" (      �     }        ��  � $  4   �  � =$     � N$  	   � X$     � h$     � u$     � �$  O   � �$     � �$     %      
            � �$  '   " (   �� %  	   8    " (   �� $%   �� &%         " (   �� 5%   �� 8%  
       " (   �� C%   �� S%     
" ( 	 
   � 
" ( 	 
 �     
" ( 	 
 �     
�             �G8    " (   �� j%   �%     tvNodeDropEnd   " (     " (     �            B� L"      �       	     B� L"      �       
     B� L"      �             B" )     �    }        �� _#     " )     " +   �&    &    * *   �       
     B" *     �            B" *     <      %                  " *     � L"          %              %                   " +     %                  " +     %       d       %       d            " +   �%                , " +     %                   " +   �%              " +   �&    &     @   * *   <      %                  " *     � L"      �       	     B" +     " +   �&    &    * *       %              %                   " +     %                   " +     �    " *     � �%     �    " *     � �%    T   " +     " *     � �%     %              %               " *     4        %              &    &     ,   %                  S    &    " ,     &    " ,   �&    &     X   * %   ( (       "  
    %                   " %     %               " ,   �$    4         %              &     *    " ,     %              " %     %              ((       " %     %              � �#     � �#     %               8   %              $    4         %              � L"      %              " ,     &    &    &    &        %              %              * -   " -     %              4       %              4       %              �    }        �� L"                      �           �   l       ��                 �  �  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       N     
                    � ߱              �  (  �      pN      4   ����pN                �                      ��                  �  �                  �m�                       �  8  �  �  �  �N            �  �  `      O      4   ����O                p                      ��                  �  �                  Hn�                       �  �  �  o   �      ,                                 �  �   �  4O      �  �   �  `O      $  $  �  �  ���                       �O     
                    � ߱        8  �   �  �O      L  �   �  �O      `  �   �  �O          $   �  �  ���                       P  @         P              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  @  �               �o�                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $      ���                       pP     
                    � ߱                  �  �                      ��                                       �c�                       4      4   �����P      $    �  ���                       �P     
                    � ߱        �      4  D      �P      4   �����P      /    p                               3   ����Q  �  �   1  Q          O   >  ��  ��  HQ                               , �                          
                               �      ��                            ����                                            �           �   l       ��                 �    �               p[�                    O   ����    e�          O   ����    R�          O   ����    ��      8c                         � ߱          $    �   ���                           p     @c  (            �     Lc                �                      ��                  
                    �k�                       
  8    /     �     �                          3   ����`c                                 3   ����|c  P     
   @                      3   �����c  �        p                      3   �����c         
   �  �                  3   ����e      $     �  ���                               
                    � ߱        �  /	    4     D  4e                      3   ����e  t        d                      3   ����@e            �                      3   ����Te  @  /	    �     �  �e                      3   ����he                                 3   �����e            0                      3   �����e    /     l     |                          3   �����e  �     
   �                      3   �����e  �        �                      3   �����e         
   �                      3   �����e      /     8     H                          3   �����e  x     
   h                      3   ����f  �     
   �                      3   ���� f            �                      3   ����4f               ,            $                                                 ��                              ��        �                  ����                                            �           �   l       ��                 #  j  �               �6�                    O   ����    e�          O   ����    R�          O   ����    ��      D"                      �          L    ,  �   t  �  Hf      4   ����Hf                �                      ��                  ,  A                  (7�                       ,    H  $  .  �  ���                       hf                         � ߱              X      �          �  �      ��                  0  6  �              �7�                     0  �      �  �       ��                            7   ����           ��                     �            $                  6   0         H   ��                    �            $                                                                �  �                                   @            d   t        O   ����  e�          O   ����  R�          O   ����  ��          /  1       (  �f                      3   ����tf  X        H                      3   �����f  �        x                      3   �����f  �        �                      3   �����f  �        �                      3   �����f                                  3   �����f  �  �   8           �      8  �        �      ��                  9  ?                 $U�                       9  (      �          ��                            7   ����    !      ��                     �            p                  6   9       ! �   ��                    �            p                                                                �  �                                   @            �   �        O   ����  e�          O   ����  R�          O   ����  ��      H  9   :     4  �   ;  !      h                    	 	 	                                              
 
 
                           �f                     �f       	       	           � ߱            $  <  �  ���                                     �                      ��                  B  f                  �s�                       B  `  4	  $  I  	  ���                       �f      "                   � ߱        �	  $   K  `	  ���                       g  @         g              � ߱        �	  $   L  �	  ���                       <g  @         (g              � ߱        �
  $   M  
  ���                       \g  @         Hg              � ߱              �
      X          (        ��                  P  e  @              �E�                       P  <
      �
  4       ��                            7   ����          ��                     �            �                  6   P        �   ��         �        �            �                                                        hg                 �  �       	    tg           |g                      �   �        O   ����  e�          O   ����  R�          O   ����  ��            Q  t  �      �g      4   �����g                                       ��                  Q  d                  ��                       Q  �  X  $  S  ,  ���                       �g      "                   � ߱        �  $  T  �  ���                       �g      "                   � ߱          $  U  �  ���                       �g      "                   � ߱        �    W  $  4      �g      4   �����g      $  W  `  ���                       h      "                   � ߱            X  �  �      (h      4   ����(h      $  X  �  ���                       Hh      "                   � ߱        �    Y  ,  <      Th      4   ����Th      $  Y  h  ���                       th      "                   � ߱            Z  �  �      �h      4   �����h      $  Z  �  ���                       �h      "                   � ߱        �    [  4  D      �h      4   �����h      $  [  p  ���                       �h      "                   � ߱             \  �  �      �h      4   �����h      $  \  �  ���                       �h      "                   � ߱            /  ^  L     \  i                      3   ����i  �        |                      3   ����$i  �        �                      3   ����0i  �        �                      3   ����<i                                3   ����Hi            <                      3   ����Ti      �   h  `i                  "  �                                               �          �  �    �                                                 "   ��                             ��                             ��                              ��        �                   ��                            ����                                =   ?                     �           �   l       ��                  p  }  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��           z  �   �       �i      4   �����i      n   {     �          �i        |    ,      �i      4   �����i      �   |  �i    ��                            ����                                            4          �   l       ��                  �  �  �               O�                    O   ����    e�          O   ����    R�          O   ����    ��      �i  �            j  �          j  �          j  �          $j  �          0j  � 	         <j  � 
             � ߱        �  Z   �  �    �        �i                  �              �              �              �              �              � ߱        �  h   �  `   �        Hj              �  s   �                                  D  �  d                               7   ����           ��                     �            �                  6   �            ��                    �            �                                                                P  D                                   @            $   4        J   �        ���    ��                                                          Tj                      �          �  s   �                                 8  �  X                               7   ����           ��                     �            �                  6   �         �   ��                    �            �                                                                D  8                                   @               (        J   �        ���    ��                                                          xj                      �              
   �  �� �             �j    ��                              ��        �                  ����                            �        2                 S�    �       2                 S�                    �           �   l       ��                  �  �  �               @��                    O   ����    e�          O   ����    R�          O   ����    ��      �     �  �j  }          O   �  ��  ��  �j    ��                            ����                                            �           �   l       ��                  �  �  �               p��                    O   ����    e�          O   ����    R�          O   ����    ��      �   /   �  �                                 3   �����j      /   �       ,                          3   �����j            L                      3   ���� k    ��                            ����                                            �           �   l       ��D               �    �               �w�                    O   ����    e�          O   ����    R�          O   ����    ��      0  �   �                   @                      ��                  �                    �}�                       �  �   �  $  �  l  ���                       k      #                   � ߱        �  $  �  �  ���                        k      #                   � ߱        H  $  �    ���                       4k      #                   � ߱        �  �   �  Tk            �      �  T      l  T      ��                  �  �  �              �~�                8	     �  \        T       ��                            7   ����    $      ��               �k    �            �                  6   �       $ �   ��         �  �k    �            �                                                        tk   �k   �k   �k                   @  4       	    �k  �k  �k           �k  �k  �k                                 O   ����  e�          O   ����  R�          O   ����  ��      �  9   �     Ll                     Xl                     dl                     pl                     |l                     �l                     �l                         � ߱        �  $  �  �  ���                       d  A  �       %    ��         �                                             �l                 P  D           �l           �l         �            $   4    �    �  �  �      �l      4   �����l                T                      ��                  �  �                  @��                       �  �  �l                     �l                         � ߱            $  �    ���                       8  A  �         �   ��         �                                             m                 $             $m           ,m         �            �             �  T  �      4m      4   ����4m                	                      ��                  �  �                  Й�                       �  d  <m                         � ߱            $  �  �  ���                       t	  /  �  d	         &                      3   ����Hm  �	  $  �  �	  ���                       hm      #                   � ߱        $
  $  �  �	  ���                       �m      #                   � ߱        |
  $  �  P
  ���                       �m      #                   � ߱        �
  $     �
  ���                       �m      #                   � ߱        ,  $       ���                       n      &                   � ߱        P  /    X     h  xn                      3   ����Xn  �     
   �                      3   �����n  �        �                      3   �����n            �  �                  3   �����n      $     $  ���                                &                   � ߱        t  /    |     �  �n                      3   �����n  �     
   �                      3   �����n  �        �                      3   �����n                                3   �����n      $     H  ���                                &                   � ߱        �  �     o  �  �     o          	     �                                      �  3   ����4o  �  3   ����@o      3   ����To              #  d                                             &  �          �  �   @ l             �      
                                                 0              0   �  # &   ��                              ��        �                   ��                            ����                            L         %      =   �                     �           �   l       ��                    *  �               x��                    O   ����    e�          O   ����    R�          O   ����    ��      �#   '                   �          H  /   '                                 3   ����`o            8                      3   ����|o      /   (  t     �                          3   �����o            �                      3   �����o             '            �       �                                        '     ��                            ����                                                      �   l       ��                 0  t  �               @�                    O   ����    e�          O   ����    R�          O   ����    ��      �#   (    �              �          �#   (                   �              p   C  �o     8  q  �  0     �o      /   D  \     l                          3   �����o            �                      3   �����o    �     �o      /   E  �     �                          3   ���� p                                  3   ����p  �  �     (p                �                      ��                  G  L                  �ը                       G  (    /  H  �     �                          3   ����4p                                   3   ����Tp        I  ,  <  T  `p      4   ����`p      O   I  ��  ��  xp      	  J  �                                    �  3   ����|p      3   �����p    �     �p  �p  �p  �p  �p      	   T  �                                          3   �����p  �       �p      	  V  P                                    `  3   �����p  p  3   �����p      3   ���� q      �      q                                      ��                  Z  n                  `֨                       Z  �  P    ^  (  8      ,q      4   ����,q      O   ^  ��  ��  Lq  �    a  l  |      Xq      4   ����Xq      O   a  ��  ��  xq       d  �  ,      �q      4   �����q                <                      ��                  d  j                  ר                       d  �  �  �  h  �q      T         
   t  �                  3   �����q      $   h  �  ���                               
 ( 	       	           � ߱              i  �        �q      4   �����q      O   i  ��  ��  �q      O   m  ��  ��  �q        p  T  d      �q      4   �����q      /   p  �     �                          3   ����r  �        �                      3   ����8r            �                      3   ����Dr             ( 	 �          �  �  $ � 4                                                                                                              
             
 $   4   D   T   d   t   �   �      
 $   4   D   T   d   t   �   �          �   (     ��                              ��        �                  ����                                            �       (  �   l   8  ��               z  �  �               (��                    O   ����    e�          O   ����    R�          O   ����    ��      �#   )                   �          �   �   �     h  �   �                   �                      ��                  �  �                  �Ψ                0     �  �           
       
           � ߱        �  $   �  x   �                       (  $   �  �  ���                       dr  @         Pr              � ߱        �  $   �  T  ���                       �r  @         pr              � ߱        �  $   �  �  ���                       �r  @         �r              � ߱            $   �    ���                       �r  @         �r              � ߱        D  �   �  �r      �  $  �  p  ���                       �r      +                   � ߱        T  A  �       * �   ��         �                                             �r                 @  4           s           s                         $    �
    �  p  �      s      4   ����s                �                      ��                  �  �                  Ш                       �  �  T  $   �  (  ���                       4s  @          s              � ߱        �  $   �  �  ���                       Ts  @         @s              � ߱              �  �  D  �  `s      4   ����`s                                        ��                  �  �                  �Ш                       �  �                <                      ��                  �  �                  ��                       �  T    L      �  T          $        ��       0         �  �  <              8�    +   t            �  �      $  �  x  ���                       �s      +                   � ߱        �  $  �  �  ���                       �s      +                   � ߱            4   �����s      O   ����  e�          O   ����  R�          O   ����  ��      �    �  p  �      4t      4   ����4t      O   �  �� ��      �  $  �  �  ���                       `t      +                   � ߱        �	  A  �       * L	   ��        	 @	                                             �t                 �	  �	           �t           �t                      h	   x	          �  �	  @
      �t      4   �����t                P
                      ��                  �  �                  ���                       �  �	  �
  $   �  |
  ���                       4u  @          u              � ߱            O   �  �� ��      x  A  �       *    ��        
                                              @u                 d  X           Lu           Tu                      8   H    �    �  �        \u      4   ����\u                �                      ��                  �  �                  �                       �  �    �      �  �  `      t  \      ��       0         �  �  �              ��    +  �u            �         $  �  �  ���                       du      +                   � ߱        L  $  �     ���                       �u      +                   � ߱            4   �����u      O   ����  e�          O   ����  R�          O   ����  ��      �  9   �     v          <v             � ߱        x  $  �  �  ���                       	      �      4  H        �      ��                  �  �                �                       �        �         ��                            7   ����    ,      ��               �v    �            T                  6   �       , �   ��         x  �v    �            T                                                        Pv   dv   pv                 �  �           �v           �v                      �   �        O   ����	 	 e�          O   ����	 	 R�          O   ����	 	 ��      �  A  �       % �   ��         �                                             �v                 �  �           �v            w         �            �   �          �    �      w      4   ����w  
              �                      ��                  �  �                  ��                       �    L  A  �       �   ��         �  |w                                         pw                 8  ,                                   @                   �    �  h  �      �w      4   �����w                �                      ��                  �  �                  `�                       �  x  0  9   �     �w          �w             � ߱        �  $  �    ���                       �w          �w         �w          Dx             � ߱            $  �  \  ���                             �  �  h      Xx      4   ����Xx                x                      ��                  �  �                  ��                       �  �  @  A  �       - �  	 ��         �  �x                                         �x   �x                   ,         	     �x  �x      	     �x  �x         �            �             �  \  l      y      4   ����y      $  �  �  ���                       $y          0y             � ߱        �  s   �  �                                  l  <                               7   ����           ��                     �            �                  6   �         �   ��                    �            �                                                                (                                     @            �           J   �        ���    ��                                                          Dy                      �          �  s   �  �                                 `  0                               7   ����           ��                     �            �                  6   �         �   ��                    �            �                                                                                                     @            �            J   �        ���    ��                                                          hy                      �              �   �  �y                  +  (                                             )  L          <  D    ,                                            ) * +     ��                              ��        �                   ��                             ��                             ��                            ����                               - 	 X  %      8   �  *       8   �  *       =   �               =   �     �        2                 S�    �       2                 S�        �          �  4   �H                              
 �                                                                    [     �  2       &                                      �                                                                                                                                        �+          �  �
   �P                              
 �                                                                 D  [     �       K&                                    
 �                                                                D  [     �  P     �&                                    
 �                                                                D  [     �         *&                                      �                                                                                                                                       
   d d     �   ��J4,
K4  � �                                               �                                                                        d     D                                                                 P   �%l �Q                                                           1&  G   
 X  �%l xd          �                                              �     �      P   |.l �Q                                                           :&  G   
 X  |.l xd         �                                              ~     �      H  l � �                                �          �           H  l ��+                                �          ,          \  <(A	�l                                 9                 ?&                @      P   �0A	�Q                                                           K&  G   
 X  �0A	�d         d                                              a     �      h  � I	%M                                                        �     �     Q&               \  h
�l                                 L                 g&                @      P  ���	�2                                                           ~&  G    X  ��	�E         �                                  
           �     �      P  ��#�	�2                                                           �&  G    X  �#�	�> 	        x                                             m     �      P  �`	
J2                                                           �&  G    X  `	
�> 
                                                       �     �       D                                                                    TXS appSrvUtils perfile-w-report Tabla de Reportes Task-No Llave-C Campo-D Campo-F Campo-I Campo-C Llave-I Llave-F Llave-D Campo-L tPF-G002 Opciones de Men�s Aplic-Id CodMnu Etiqueta Tipo SUB-MENU SEPARADOR LINEA PROCESO Programa Acceso-directo Icon Seguridad-Grupos Seguridad-Atributos Persistente Tecla-Aceleradora app-id usuario-w-report ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST x-aplic tUsuarios tUsuario tNombre testado tLogUso tCodModulo tNomModulo tDia tHora wWin h_pure4gltv BUTTON-Excel-Users BUTTON-Excel-Users-2 FILL-IN-app FILL-IN-heredado FILL-IN-menu FILL-IN-modulo FILL-IN-nodekey FILL-IN-run TOGGLE-user-activos BROWSE-10 X(15) X(80) BROWSE-9 X(50) fMain X(256) yes/no GUI MENU GENERAL DEL ERP PROGRESS DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   BROWSE-9 BROWSE-10 BUTTON-Excel-Users TOGGLE-user-activos BUTTON-Excel-Users-2 CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE c-csv-file c-xls-file x-file 99/99/9999 HH:MM:SS - / : _   .xlsx Se grabo el archivo  iStartPage ADM-ERROR currentPage pure4gltv.w wineModeAutomaticwindowsSkinAutomaticpicCacheCoef1labCacheCoef1tvIterationHeight17TreeStyle3FocSelNodeBgColor1UnfSelNodeBgColor8tvnodeDefaultFont1FocSelNodeFgColor15UnfSelNodeFgColor0resizeVertical?resizeHorizontal?DragSourcenoneautoSortnoMSkeyScrollForcePaintyesHideOnInitnoDisableOnInitnoObjectLayout tvNodeEvent AFTER ADM-CREATE-OBJECTS pParent  PF-G003 Aplicaciones tvpics/column.bmp addOnExpand PF-G002 Opciones de Men�s x-menu x-icon x-expanded x-label tvpics/separator.bmp tvpics/table.bmp tvpics/menusubmenu.bmp tvRefresh CARGAR-TREEVIEW DISABLE_UI ENABLE_UI EXITOBJECT INITIALIZEOBJECT x-programa x-modulo x-desde GENERAL logtabla Log de Tablas _User INACTIVO ACTIVO LogAccesos_ LOGACCESOS pcnodeKey TVNODEADDONEXPAND pcEvent nCust norder cSalesrep icustnum iorder select rightClick tvNodeCreatePopup failed with the following message: MenuAddChildNode MenuAddSR MenuAddCustomer MenuAddOrder MenuAddOrderLine addToEdMsg('Menu item event fired: ' + pcEvent + ' for key ' + pcnodeKey + '
') MenuHelloWorld Hello World! Node key parent of the popup menu item: DragBegin k dropOnYourself n4 cancelDrag n1 hTargetFrame getOtherWinTargetFrame DropEnd, TVNODEEVENT x-tpf-g002 x-conteo x-node-key , PF-G004 Aplicaciones por Usuario gn-users Usuarios IDX01 TVNODESELECT REPO01 REPO02 REPO03 default Perfil / Grupo Cod.User Nombre usuario Estado Node Key Menu Excel Users Aplic Solo usuarios activos Log de accesos a Excel Programa Heredado Modulo Llave02 _Userid Llave01 �  `+    2      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
 pcProcName  �   ��      �         pcProcName      ��              
 pcProcName      ��      $        piPageNum       ��      H        piPageNum       ��      l        pcPageList      ��      �        pcProc  �  ��      �        pcLinkName      ��      �        pcLinkName    ��      �       
 phTarget        ��              phTarget        ��      @        piPageNum       ��      d        pcValue     ��      �        piPageNum       ��      �        pcAction        ��      �       
 phAppService        ��      �        pcMode     ��             
 phSource    D  ��      8        phSource        ��      \       
 phSource    �  ��      �        pcText  �  ��      �        pcText      ��      �        pcText  �  ��      �       
 phObject      ��             
 phObject        ��      (        phObject        ��      L        pcField     ��      l        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller    �  ��      �        phCaller        ��      �        phCaller    (  ��               pcMod   H  ��      @        pcMod       ��      `       
 pcMod   �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    �  ��      �        pdRow       ��              pdRow       ��      ,       
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   �	  �	  �	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc                1  >  @  H  �     9                                     �  	     :                                       �  L	     ;                                       	  �	     <                                   S  T	  �	     =                                   Z  [  �	  �	     >                                   f  g  h  
        
  
   hProc   8
        ,
     c-csv-file  X
        L
     c-xls-file           l
     x-file  �	  �
     ?   �	                              s  t  u  v  y  }  �  �  �  �  �  �  �  �  �  �  t
       @                                   �  �  �
  L     A                                   �  �  �  �    �     B                                   �  �            �     currentPage \  �     C   �          �                  adm-create-objects      
                  @  "     8     x-menu  \  "     T     x-icon  |  "     p     x-expanded      "     �     x-label           �        pParent �  �  "   D   $  �      �                  cargar-treeview ,  .  0  1  6  8  9  :  ;  <  ?  A  B  I  K  L  M  P  Q  S  T  U  W  X  Y  Z  [  \  ^  d  e  f  h  j  �  �     E               �                  disable_UI  z  {  |  }  �       F               �                  enable_UI   �  �  �  �  �  �  X     G               L                  exitObject  �  �  �    �     H               �                  initializeObject    �  �  �  �  #      �     x-programa  �  #      �     x-modulo      #           x-desde ,  &     $  
   hProc   L  &     @     c-csv-file  l  &     `     c-xls-file      #      �     x-file  d  �     I   �          �                  LogAccesos  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �                         '      X        pcnodeKey   �  �     J       @      �                  tvNodeaddOnExpand   '  (  *  �  (      �     nCust   �  (      �     norder    (            cSalesrep   ,  (            icustnum    H  (      @     iorder      (   	   \  
   hTargetFrame    �  (      �        pcEvent     (      �        pcnodeKey   d  �     K   �  l      �                  tvNodeEvent C  D  E  G  H  I  J  L  T  V  Z  ^  a  d  h  i  j  m  n  p  q  t  d  +     X     x-conteo        +     x     x-node-key      )      �        pcnodeKey        * B  �  x-tpf-g002  �    7   L   D  �  �  �                  tvNodeSelect    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �         <      �                      �     4  
   perfile-w-report    �         �         �         �        �        �        �        �         �         �        Task-No Llave-I Llave-C Campo-D Campo-F Campo-I Campo-C Llave-F Llave-D Campo-L D         tPF-G002    �         �         �         �         �         �         �         �                           (         <         CodMnu  Etiqueta    Tipo    Programa    Seguridad-Grupos    Acceso-directo  Icon    Aplic-Id    Seguridad-Atributos Persistente Tecla-Aceleradora   app-id  0  T  h  
   usuario-w-report    �         �         �         �                                                    (        Task-No Llave-I Llave-C Campo-D Campo-F Campo-I Campo-C Llave-F Llave-D Campo-L �  @  L     tUsuarios   p         |         �         tUsuario    tNombre testado     �  �     tLogUso �                                              ,         4         tCodModulo  tNomModulo  tDia    tHora   tUsuario    tNombre testado \          P  
   appSrvUtils x       p     x-aplic �       �  
   wWin    �       �  
   h_pure4gltv �       �     FILL-IN-app �       �     FILL-IN-heredado                 FILL-IN-menu    D       4     FILL-IN-modulo  h       X     FILL-IN-nodekey �    	   |     FILL-IN-run �    
   �     TOGGLE-user-activos �        �  
   gshAstraAppserver      	 	     �  
   gshSessionManager   $  
 
       
   gshRIManager    L        8  
   gshSecurityManager  t        `  
   gshProfileManager   �        �  
   gshRepositoryManager    �        �  
   gshTranslationManager   �        �  
   gshWebManager                gscSessionId    8        (     gsdSessionObj   \        L  
   gshFinManager   �        p  
   gshGenManager   �        �  
   gshAgnManager   �        �     gsdTempUniqueID �        �     gsdUserObj          �     gsdRenderTypeObj    8        $     gsdSessionScopeObj  T       L  
   ghProp  t       h  
   ghADMProps  �       �  
   ghADMPropsBuf   �       �     glADMLoadFromRepos  �       �     glADMOk �       �  
   ghContainer             cObjectName 8       0     iStart  X       L     cAppService x       l     cASDivision �       �     cServerOperatingMode    �       �     cFields          �     iStartPage      \  �  perfile-w-report         \    tPF-G002    D    \  0  usuario-w-report    `    L  T  tUsuarios   x    L  p  tLogUso �        �  PF-G003 �   !    �  PF-G002 �   $    �  logtabla    �   %    �  _User   �   ,    �  PF-G004      -      gn-users             D   �  �  �          
      �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �   	  	  	  	  	  	  	  		  
	  	  	  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	   
  
  
  
  
  
  
  |
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
    "  #  &  '  (  )  +  ,  .  /  0  1  2  3  4  5  7  8  9  :  ;  <  >  ?  @  A  B  C  D  E  F  G  H  I  J  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _  `  a  b  c  �  �      #  $  (  )  *  ,  /  9  U  g  �  �  �  �  G  _  `  z  �  �  �  �  �  �  �  �  �  �  �    �  �  �  	  
          ,  -  .  0  8  >  D  G  L  P  Q  R  T  X  �  �  *  +  U  b  p  �  �  �  �  �  �  �  �  �  �  �  �  �  �      H� $ C:\Progress\OpenEdge\src\adm2\windowmn.i $#  f!  C:\Progress\OpenEdge\src\adm2\containr.i X#  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �#  ��  C:\Progress\OpenEdge\src\adm2\visual.i   �#  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  $  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    D$  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   |$  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �$  Ds   C:\Progress\OpenEdge\gui\fn  �$  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   %  Q.  C:\Progress\OpenEdge\gui\set \%  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i �%  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �%  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �%  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  @&  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i t&  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �&  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i �&  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    ('  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    l'  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �'  �j  C:\Progress\OpenEdge\gui\get �'  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    (  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    P(  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �(  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �(  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �(  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   <)  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �)  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �)  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  �)  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  0*  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i t*  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �*  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �*  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   (+  j�    &D:\xpciman\progress\Menu-ERP-Progress\Menu-ERP.w     �  �      �+     �  $   �+  $        �+  �        �+     �     �+  �   �     �+     �     �+  �   �     ,     ]  #   ,  �   G     ,,     E      <,  �   >     L,     <      \,  �   ;     l,     9      |,  r        �,  n        �,     �  "   �,  i   �     �,     �     �,  P   m     �,  �   d     �,       !   �,  �        -     �     -  �   �     ,-     �     <-  �   �     L-     �     \-  g   �     l-     e     |-  O   M     �-  �   �     �-     �      �-  �   �     �-     M     �-  �   B     �-           �-  �        �-     �     .  �   �     .     �     ,.  �   �     <.     �     L.  �   �     \.     �     l.  �   �     |.     _     �.  }   S     �.     1     �.     �     �.     g     �.          �.  7   �     �.  �   �     �.  O   �     /     �     /     g     ,/  �        </  �        L/  O        \/     �
     l/     �
     |/  �   �
     �/  x   |
  
   �/  M   g
     �/     V
     �/     

     �/  a   �	  
   �/  �  �	     �/     �	     �/  �  �	     0  O   r	     0     a	     ,0     	     <0  �   =     L0          \0     d     l0  x   ^     |0     E     �0     �     �0     �     �0     �     �0     �     �0  Q   �  
   �0     1     �0     �  
   �0     �     1     �  
   1  f   �     ,1     A  	   <1  "   �     L1     �     \1     �     l1  Z   w     |1          �1     @     �1     ,     �1          �1     �     �1  4   �       �1     M      �1     !       �1           