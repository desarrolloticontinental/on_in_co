	��V6�a05  7�              +                                )� 3530010Cutf-8 MAIN d:\newsie\on_in_co\APLIC\ccb\d-msgblo.w,,INPUT pCodCli CHARACTER,INPUT pNroCard CHARACTER,OUTPUT pRpta CHARACTER PROCEDURE Log-de-control,, PROCEDURE initializeObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE Clave-Administrador,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER      0              ��              Œ 0  ��              `d              d&    +   Q �  7   �U `  8   Y �   ?   Z $
  @   4d 8  A   le �  B   \g �  C   �i T  D           4m �  o   ? 4r �   iSO8859-1                                                                           D    �                                       �              �  ��                �  �    �   {�    Ć  �         8�  �   �                                                             PROGRESS                         �           
    
                                  �                                                                                                     
  �       �             �         �       �             �         �                      �         �             |                                                                                          �             \             �                                                                                          �                          INTEGRAL                         PROGRESS                         l     �        �                         �ɺ[            �  �l                              �  �                      �  �  �       CODCIACLVMODPORIGVDIAS-RESHORA-RESMINU-RESBRRPEDCLIVARTPOCMBITEMS_FACTURAITEMS_BOLETAITEMS_GUIASITEMS_PEDIDOITEMS_N_CREDITOITEMS_N_DEBITODTOMAXDTODISDTOMAYMRGPUBDTOPROITEMS_PEDMOSCLA_COMPRACLA_VENTACODCTAMRGMINMRGMAYMRGDISFACPORALMALTTOLVENROUNDOCUPORMORA                                                                       	          
                                                                                                                                                                                                                                     !          @     �        �                         �\            �  '�                              �  �                      P	  �  RT     CODCLINOMCLIDIRCLIRUCTPOCLILOCCLIFAXCLIFCHINGFLGSITCODPOSCODCIAREFERENCIASCODPAISCODDEPTCODPROVCODDISTGIRCLICNDVTATELFNOSE-MAILFCHACTCLFCLIUSUARIOREPLEGFNREPRTRANSPORTEMONLCIMPLCCODVENDIRENTFCHCESCONTACFCHVLCCODANTUSRLCVENANTFLAGAUTUSRAUTOBSAUTCODDIVDEPENTPROENTDISENTLINCREUSRCRETOTUSOFCHAUTAVAL1AVAL2TLFLOGJFELOGDNIRUCOLDCANALCLFCOMNROCARDDIRREFCODUNICOAPEPATAPEMATNOMBRECOB_DIASCOB_HORARIOCOB_DIRECCIONCOB_CARTACOB_GLOSACODIBCCLFCLI2LIBRE_C01LIBRE_C02LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_L01LIBRE_L02FLAGTIPOFLAGUSUARIOFLAGMIGRACIONFLAGFECHACM_CLFCLI_PCM_CLFCLI_TSWBAJASUNATSWCARGASUNAT                                                                        	          
                                                                                                                                                                                                                                    !          "          #          $          %          &          '          (          )          *          +          ,          -         .         /          0         1         2         3          4         5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          Q          R          S          T          U          V          �                                      �ɺ[              ~}                              �  �                      $  �  Q      CODCIACODCLIFECHAUSUARIOFCHVTOMENSAJEFCHINIHORAORIGENCODUNICONROMSGTIPOIMPMNIMPME                                                                         	          
                                                            x  /
      �  
    
                  �  �             d                                                                                          /
          
  $  A
      �  
    
                  �  T  	                                                                                                     A
          
  �  S
      L  
    
                  8     
           �                                                                                          S
          
  |  `
      �  
    
                  �  �             h                                                                                          `
          
  (  s
      �  
    
                  �  X                                                                                                       s
          
  �  �
      P  
    
                  <               �                                                                                          �
          
  �  �
      �  
    
                  �  �             l                                                                                          �
          
  ,  �
      �  
    
                  �  \                                                                                                       �
          
  �  �
      T                         @               �                                                                                          �
            �  �
                               �  �             p                                                                                          �
            0  �
      �  
    
                  �  `                                                                                                       �
          
  �  �
      X  
    
                  D               �                                                                                          �
          
  �  �
        
    
                  �  �             t                                                                                          �
          
  4        �                        �  d                                                                                                                    �        \                        H               �                                                                                                      �                                �  �             x                                                                                                          /      �                        �  8             $                                                                                          /                                                    �ɺ[              9�                              �  �                      p  �  �      CODCIACODDOCDESCIPPSSWCREATEPSSWUPDATEPSSWDELETEFCHMODHRAMODUSUARIOCHR__01CHR__02CHR__03CHR__04CHR__05DEC__01DEC__02DEC__03DEC__04DEC__05DATE__01DATE__02LOG__01LOG__02                                                                       	          
                                                                                                                                                                   	 �                                               �          �    < �                         
             
             
                                         
                                                                                                                <   L   \   l   |   �   �   �   �   �   �   �   �         <   L   \   l   |   �   �   �   �   �   �   �   �      ��                                               \          ����                            \    ,�                    �    r    �x    z    �    �    �    �    ��    undefined                                                               �       0�  �   l   @�    P�                  �����               `E1                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       p
     <          assignFocusedWidget         �      �            LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    $       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    6       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          L       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    X       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    d       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    w       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �       LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
       LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �          HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    #      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    7      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    E      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    U      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    f      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    s      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    �
  $  �   �
  ���                       d                          � ߱        �  A   �        $   ��                                                      p                  l  `           |            �          �            @   P    h    �   �    �  �       4   �����                 (                      ��                  �   �                   |�0                       �   �  l    �   D  T      �       4   �����       O   �   ��  ��  �       A  �         �   ��         �  �                                          �                               �            �                       �   �                  �                      ��                  �                     ��0                       �   $      A  �            ��         �  p                                        8   D                   T  H           P  `           X  h         �                4    �      �  �      �      4   �����      O     ��  ��  �    $    �  ���                       �                         � ߱        �  B          h   ��         T  ,                                         �                       �  �                          $                      �   �        
  �  �      \      4   ����\      O   
  ��  ��  h  T      ,  <      |      4   ����|      O     ��  ��  �  �      p  �      �      4   �����      $    �  ���                       �                         � ߱        �    �  �  p      $      4   ����$                �                      ��                  �  �                  tB.                       �        �  �  �      <      4   ����<      $  �  �  ���                       �  @         l              � ߱              �     0      �      4   �����      $  �  \  ���                       �  @         �              � ߱        assignPageProperty                                       ��                  !  $  8               E.                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             P               ��                  x           ��                            ����                            changePage                              p  X      ��                  &  '  �              �c.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             p  X      ��                  )  +  �               �0                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            constructObject                             �  �      ��                  -  2  �              `0                    O   ����    e�          O   ����    R�          O   ����    ��            ��                 �               �� 
  (             �  
             ��   P                            �� 
                 D  
         ��                            ����                            createObjects                               @  (      ��                  4  5  X              ��/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              @  (      ��                  7  9  X              8�/                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  p           ��                            ����                            destroyObject                               l  T      ��                  ;  <  �              D�.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                l  T      ��                  >  @  �              ��.                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            initializeObject                                �  �      ��                  B  C  �              �j1                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               �  �      ��                  E  F  �              4k1                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               �  �      ��                  H  J  �              �k1                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            notifyPage                              �  �      ��                  L  N  �              H61                    O   ����    e�          O   ����    R�          O   ����    ��            ��                              ��                            ����                            passThrough                             �   �       ��                  P  S  !              ��/                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��                  T!           ��                            ����                            removePageNTarget                               T"  <"      ��                  U  X  l"              �-                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �"             �"  
             ��                  �"           ��                            ����                            selectPage                              �#  �#      ��                  Z  \  �#              |80                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �#           ��                            ����                            toolbar                             �$  �$      ��                  ^  `  �$              \{1                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �$           ��                            ����                            viewObject                              �%  �%      ��                  b  c  &              @�.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                �&  �&      ��                  e  g  '               (�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                   '           ��                            ����                            disablePagesInFolder    
      �'      �'    �      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder �'      �'       (    �      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure   (      L(      �(    �      HANDLE, getCallerWindow `(      �(      �(    �      HANDLE, getContainerMode    �(      �(      �(    �      CHARACTER,  getContainerTarget  �(       )      4)    �      CHARACTER,  getContainerTargetEvents    )      @)      |)    �      CHARACTER,  getCurrentPage  \)      �)      �)          INTEGER,    getDisabledAddModeTabs  �)      �)      �)     !      CHARACTER,  getDynamicSDOProcedure  �)      *      @*  !  8      CHARACTER,  getFilterSource  *      L*      |*  "  O      HANDLE, getMultiInstanceActivated   \*      �*      �*  #  _      LOGICAL,    getMultiInstanceSupported   �*      �*      +  $  y      LOGICAL,    getNavigationSource �*      +      H+  %  �      CHARACTER,  getNavigationSourceEvents   (+      T+      �+  &  �      CHARACTER,  getNavigationTarget p+      �+      �+  '  �      HANDLE, getOutMessageTarget �+      �+      ,  (  �      HANDLE, getPageNTarget  �+      ,      D,  )  �      CHARACTER,  getPageSource   $,      P,      �,  *  �      HANDLE, getPrimarySdoTarget `,      �,      �,  +        HANDLE, getReEnableDataLinks    �,      �,      �,  ,        CHARACTER,  getRunDOOptions �,      -      8-  -  /      CHARACTER,  getRunMultiple  -      D-      t-  .  ?      LOGICAL,    getSavedContainerMode   T-      �-      �-  /  N      CHARACTER,  getSdoForeignFields �-      �-      �-  0  d      CHARACTER,  getTopOnly  �-      .      0.  1 
 x      LOGICAL,    getUpdateSource .      <.      l.  2  �      CHARACTER,  getUpdateTarget L.      x.      �.  3  �      CHARACTER,  getWaitForObject    �.      �.      �.  4  �      HANDLE, getWindowTitleViewer    �.      �.      (/  5  �      HANDLE, getStatusArea   /      0/      `/  6  �      LOGICAL,    pageNTargets    @/      l/      �/  7  �      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject |/      �/      0  8  �      LOGICAL,INPUT h HANDLE  setCallerProcedure  �/      0      P0  9  �      LOGICAL,INPUT h HANDLE  setCallerWindow 00      h0      �0  :        LOGICAL,INPUT h HANDLE  setContainerMode    x0      �0      �0  ;        LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  �0      1      @1  <  (      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage   1      d1      �1  =  ;      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  t1      �1      �1  >  J      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �1      2      P2  ?  a      LOGICAL,INPUT pcProc CHARACTER  setFilterSource 02      p2      �2  @  x      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �2      �2      �2  A  �      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   �2      3      P3  B  �      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   03      �3      �3  C  �      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource �3      �3       4  D  �      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents    4      D4      �4  E  �      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget `4      �4      �4  F  �      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �4      �4      ,5  G        LOGICAL,INPUT phObject HANDLE   setPageNTarget  5      L5      |5  H  %      LOGICAL,INPUT pcObject CHARACTER    setPageSource   \5      �5      �5  I  4      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget �5      �5      $6  J  B      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    6      L6      �6  K  V      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget d6      �6      �6  L  k      LOGICAL,INPUT phObject HANDLE   setRunDOOptions �6       7      07  M  {      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  7      T7      �7  N  �      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   d7      �7      �7  O  �      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �7      8      @8  P  �      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly   8      l8      �8  Q 
 �      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource x8      �8      �8  R  �      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget �8      9      <9  S  �      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    9      `9      �9  T  �      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    t9      �9      �9  U         LOGICAL,INPUT phViewer HANDLE   getObjectType   �9      :      <:  V        CHARACTER,  setStatusArea   :      H:      x:  W  #      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             ,;  ;      ��                  �  �  D;              �˔                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               0<  <      ��                  �  �  H<              4Δ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                4=  =      ��                  �  �  L=              PP�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                <>  $>      ��                  �  �  T>              �P�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               @?  (?      ��                  �  �  X?              �f�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  p?           ��                            ����                            getAllFieldHandles  X:      �?      @  X  1      CHARACTER,  getAllFieldNames    �?      @      L@  Y  D      CHARACTER,  getCol  ,@      X@      �@  Z  U      DECIMAL,    getDefaultLayout    `@      �@      �@  [  \      CHARACTER,  getDisableOnInit    �@      �@       A  \  m      LOGICAL,    getEnabledObjFlds   �@      A      @A  ]  ~      CHARACTER,  getEnabledObjHdls    A      LA      �A  ^  �      CHARACTER,  getHeight   `A      �A      �A  _ 	 �      DECIMAL,    getHideOnInit   �A      �A      �A  `  �      LOGICAL,    getLayoutOptions    �A       B      4B  a  �      CHARACTER,  getLayoutVariable   B      @B      tB  b  �      CHARACTER,  getObjectEnabled    TB      �B      �B  c  �      LOGICAL,    getObjectLayout �B      �B      �B  d  �      CHARACTER,  getRow  �B      �B      $C  e  �      DECIMAL,    getWidth    C      0C      \C  f        DECIMAL,    getResizeHorizontal <C      hC      �C  g        LOGICAL,    getResizeVertical   |C      �C      �C  h  "      LOGICAL,    setAllFieldHandles  �C      �C      D  i  4      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �C      <D      pD  j  G      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    PD      �D      �D  k  X      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    �D      �D      E  l  i      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �D      <E      lE  m  z      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    LE      �E      �E  n  �      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout �E      �E      F  o  �      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal �E      8F      lF  p  �      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   LF      �F      �F  q  �      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �F      �F      (G  r  �      LOGICAL,    getObjectSecured    G      4G      hG  s  �      LOGICAL,    createUiEvents  HG      tG      �G  t  �      LOGICAL,    bindServer                              @H  (H      ��                  �  �  XH              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               DI  ,I      ��                  �  �  \I              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             LJ  4J      ��                  �  �  dJ              �H�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                TK  <K      ��                  �  �  lK              LI�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              `L  HL      ��                  �  �  xL              @ɔ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             hM  PM      ��                  �  �  �M              �ɔ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             lN  TN      ��                  �  �  �N              �ʔ                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �N  
         ��                            ����                            startServerObject                               �O  �O      ��                  �  �  �O              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �P  �P      ��                  �  �  �P              <��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �P           ��                            ����                            getAppService   �G      8Q      hQ  u  	      CHARACTER,  getASBound  HQ      tQ      �Q  v 
 	      LOGICAL,    getAsDivision   �Q      �Q      �Q  w  	      CHARACTER,  getASHandle �Q      �Q      R  x  *	      HANDLE, getASHasStarted �Q      R      LR  y  6	      LOGICAL,    getASInfo   ,R      XR      �R  z 	 F	      CHARACTER,  getASInitializeOnRun    dR      �R      �R  {  P	      LOGICAL,    getASUsePrompt  �R      �R      S  |  e	      LOGICAL,    getServerFileName   �R      S      DS  }  t	      CHARACTER,  getServerOperatingMode  $S      PS      �S  ~  �	      CHARACTER,  runServerProcedure  hS      �S      �S    �	      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �S      T      <T  �  �	      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   T      dT      �T  �  �	      LOGICAL,INPUT pcDivision CHARACTER  setASHandle tT      �T      �T  �  �	      LOGICAL,INPUT phASHandle HANDLE setASInfo   �T      U      0U  � 	 �	      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    U      PU      �U  �  �	      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  hU      �U      �U  �  �	      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �U      �U      0V  �  
      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  V      TV      �V  �  
      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             HW  0W      ��                  �  �  `W              �Ǔ                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �W             xW  
             ��   �W             �W               �� 
                 �W  
         ��                            ����                            addMessage                              �X  �X      ��                  �  �  �X              �֓                    O   ����    e�          O   ����    R�          O   ����    ��            ��   $Y             �X               ��   LY             Y               ��                  @Y           ��                            ����                            adjustTabOrder                              <Z  $Z      ��                  �  �  TZ              8�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �Z             lZ  
             �� 
  �Z             �Z  
             ��                  �Z           ��                            ����                            applyEntry                              �[  �[      ��                  �  �  �[              �J�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �[           ��                            ����                            changeCursor                                �\  �\      ��                  �  �  �\              �M�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  ]           ��                            ����                            createControls                              ^  �]      ��                  �  �  $^              4��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               _  �^      ��                  �  �  (_              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                `  �_      ��                  �  �  ,`              � �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                               a  a      ��                  �  �  8a              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                               b  b      ��                  �  �  8b              h�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                               c  c      ��                  �  �  8c              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                (d  d      ��                  �  �  @d              48�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              0e  e      ��                  �  �  He              L9�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �e             `e  
             ��   �e             �e               ��   �e             �e               ��                  �e           ��                            ����                            modifyUserLinks                             �f  �f      ��                  �  �  �f              �S�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   8g             g               ��   `g             ,g               �� 
                 Tg  
         ��                            ����                            removeAllLinks                              Ph  8h      ��                  �  �  hh              �B�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              Pi  8i      ��                  �  �  hi              C�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �i             �i  
             ��   �i             �i               �� 
                 �i  
         ��                            ����                            repositionObject                                �j  �j      ��                  �  �  �j              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4k              k               ��                  (k           ��                            ����                            returnFocus                              l  l      ��                  �  �  8l              �ْ                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 Pl  
         ��                            ����                            showMessageProcedure                                Tm  <m      ��                  �    lm              <ڒ                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �m             �m               ��                  �m           ��                            ����                            toggleData                              �n  �n      ��                      �n              䏒                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �n           ��                            ����                            viewObject                              �o  �o      ��                    	  �o              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  lV      <p      hp  � 
 }      LOGICAL,    assignLinkProperty  Hp      tp      �p  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �p       q      0q  �  �      CHARACTER,  getChildDataKey q      <q      lq  �  �      CHARACTER,  getContainerHandle  Lq      xq      �q  �  �      HANDLE, getContainerHidden  �q      �q      �q  �  �      LOGICAL,    getContainerSource  �q      �q      (r  �  �      HANDLE, getContainerSourceEvents    r      0r      lr  �  �      CHARACTER,  getContainerType    Lr      xr      �r  �        CHARACTER,  getDataLinksEnabled �r      �r      �r  �        LOGICAL,    getDataSource   �r      �r      (s  �  0      HANDLE, getDataSourceEvents s      0s      ds  �  >      CHARACTER,  getDataSourceNames  Ds      ps      �s  �  R      CHARACTER,  getDataTarget   �s      �s      �s  �  e      CHARACTER,  getDataTargetEvents �s      �s       t  �  s      CHARACTER,  getDBAware   t      ,t      Xt  � 
 �      LOGICAL,    getDesignDataObject 8t      dt      �t  �  �      CHARACTER,  getDynamicObject    xt      �t      �t  �  �      LOGICAL,    getInstanceProperties   �t      �t      u  �  �      CHARACTER,  getLogicalObjectName    �t      (u      `u  �  �      CHARACTER,  getLogicalVersion   @u      lu      �u  �  �      CHARACTER,  getObjectHidden �u      �u      �u  �  �      LOGICAL,    getObjectInitialized    �u      �u       v  �        LOGICAL,    getObjectName    v      ,v      \v  �        CHARACTER,  getObjectPage   <v      hv      �v  �  '      INTEGER,    getObjectParent xv      �v      �v  �  5      HANDLE, getObjectVersion    �v      �v      w  �  E      CHARACTER,  getObjectVersionNumber  �v      w      Tw  �  V      CHARACTER,  getParentDataKey    4w      `w      �w  �  m      CHARACTER,  getPassThroughLinks tw      �w      �w  �  ~      CHARACTER,  getPhysicalObjectName   �w      �w      x  �  �      CHARACTER,  getPhysicalVersion  �w      $x      Xx  �  �      CHARACTER,  getPropertyDialog   8x      dx      �x  �  �      CHARACTER,  getQueryObject  xx      �x      �x  �  �      LOGICAL,    getRunAttribute �x      �x      y  �  �      CHARACTER,  getSupportedLinks   �x      y      Py  �  �      CHARACTER,  getTranslatableProperties   0y      \y      �y  �  �      CHARACTER,  getUIBMode  xy      �y      �y  � 
       CHARACTER,  getUserProperty �y      �y      z  �  #      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �y      4z      lz  �  3      CHARACTER,INPUT pcPropList CHARACTER    linkHandles Lz      �z      �z  �  H      CHARACTER,INPUT pcLink CHARACTER    linkProperty    �z      �z      {  �  T      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry �z      P{      |{  �  a      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   \{      �{      |  �  m      CHARACTER,INPUT piMessage INTEGER   propertyType    �{      <|      l|  �  {      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  L|      �|      �|  �  �      CHARACTER,  setChildDataKey �|      �|       }  �  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  �|      (}      \}  �  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  <}      |}      �}  �  �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    �}      �}      ~  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �}      0~      d~  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   D~      �~      �~  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents �~      �~        �        LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �~      8      l  �        LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   L      �      �  �  /      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents �      �      �  �  =      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  �      @�      l�  � 
 Q      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject L�      ��      ��  �  \      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    ��      �      �  �  p      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      8�      p�  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    P�      ��      ́  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   ��      �      �  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      @�      p�  �  �      LOGICAL,INPUT pcName CHARACTER  setObjectParent P�      ��      ��  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion    ��      ��      �  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    �      <�      p�  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks P�      ��      ̃  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   ��      �      $�  �        LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �      D�      x�  �  (      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute X�      ��      ̄  �  ;      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   ��      �      (�  �  K      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   �      L�      ��  �  ]      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  h�      ��      ؅  � 
 w      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ��      ��      (�  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage �      h�      ��  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   t�      ��      �  � 	 �      CHARACTER,INPUT pcName CHARACTER    ܉      $�  ��      $      4   ����$                ��                      ��                     M                  슔                          4�        !  ̇  H�      4      4   ����4                X�                      ��                  "  L                  t^�                       "  ܇  X�    9  t�  ��      H      4   ����H                 �                      ��                  E  G                  �^�                       E  ��         F                                  �     
                    � ߱        ��  $  I  ,�  ���                           $  K  ��  ���                       8                         � ߱        �    Q  ��  t�      H      4   ����H                ��                      ��                  R  	                  �_�                       R  �  ��  o   U      ,                                 �  $   V  �  ���                       �  @         �              � ߱        $�  �   W  �      8�  �   X  P      L�  �   Z  �      `�  �   \  8      t�  �   ^  �      ��  �   `         ��  �   a  �      ��  �   b  �      ċ  �   e  L	      ؋  �   g  �	      �  �   h  <
       �  �   j  �
      �  �   k  4      (�  �   l  p      <�  �   m  �      P�  �   n  `      d�  �   t  �      x�  �   v        ��  �   |  L      ��  �   ~  �      ��  �   �  4      Ȍ  �   �  �      ܌  �   �  ,      ��  �   �  �      �  �   �        �  �   �  �      ,�  �   �        @�  �   �  @      T�  �   �  �      h�  �   �  �      |�  �   �  d      ��  �   �  �      ��  �   �  �      ��  �   �        ̍  �   �  T      ��  �   �  �      �  �   �        �  �   �  H      �  �   �  �      0�  �   �  �      D�  �   �  �      X�  �   �  8      l�  �   �  t      ��  �   �  �          �   �  �                      ��          �   �      ��                  =	  k	  0�              ���                    O   ����    e�          O   ����    R�          O   ����    ��      \     
                �                     �                         � ߱        ؏  $ Q	  H�  ���                           O   i	  ��  ��  (               D�          4�  <�    $�                                             ��                            ����                                :      ��      ��     6     L�                      V H�                       ��    �	  �  ��      4      4   ����4                ��                      ��                  �	  
                  D�                       �	  �  ��  �   �	  �      ��  �   �	        ̑  �   �	  �      ��  �   �	         ��  �   �	  |      �  �   �	  �      �  �   �	  l      0�  �   �	  �      D�  �   �	  d      X�  �   �	  �      l�  �   �	  T      ��  �   �	  �      ��  �   �	  L          �   �	  �      ��    
  Ē  @�      8      4   ����8  	              P�                      ��             	     
  �
                  t��                       
  Ԓ  d�  �    
  �      x�  �   !
         ��  �   "
  �       ��  �   #
  �       ��  �   $
  p!      ȓ  �   %
  �!      ܓ  �   &
  `"      �  �   '
  �"      �  �   (
  H#      �  �   )
  �#      ,�  �   *
  8$      @�  �   +
  �$      T�  �   ,
   %      h�  �   -
  �%      |�  �   .
  &      ��  �   /
  �&      ��  �   0
  '      ��  �   1
  �'      ̔  �   2
  (      ��  �   3
  �(      ��  �   4
   )      �  �   5
  |)      �  �   6
  �)      0�  �   7
  t*      D�  �   8
  �*      X�  �   9
  l+      l�  �   :
  �+          �   ;
  d,      ��    �
  ��  �      �,      4   �����,  
              (�                      ��             
     �
  j                  ���                       �
  ��  <�  �   �
  ,-      P�  �   �
  �-      d�  �   �
  $.      x�  �   �
  �.      ��  �   �
  /      ��  �   �
  �/      ��  �   �
  �/      Ȗ  �   �
  00      ܖ  �   �
  �0      �  �   �
  �0      �  �   �
  1      �  �   �
  �1      ,�  �   �
  2      @�  �   �
  �2      T�  �   �
  �2      h�  �   �
  h3      |�  �   �
  �3      ��  �   �
  X4      ��  �   �
  �4      ��  �   �
  5      ̗  �   �
  �5      ��  �   �
  �5      ��  �   �
  l6      �  �   �
  �6      �  �   �
  �6      0�  �   �
  `7      D�  �   �
  �7      X�  �   �
  �7      l�  �   �
  8      ��  �   �
  P8      ��  �   �
  �8      ��  �   �
  �8      ��  �   �
  9      И  �   �
  x9      �  �   �
  �9      ��  �   �
  �9      �  �   �
  ,:       �  �   �
  h:      4�  �   �
  �:      H�  �   �
  �:      \�  �   �
  ;      p�  �   �
  �;      ��  �   �
  <      ��  �   �
  x<      ��  �   �
  �<      ��  �   �
  h=      ԙ  �   �
  �=      �  �   �
  `>      ��  �   �
  �>      �  �   �
  X?      $�  �   �
  �?      8�  �   �
  @      L�  �   �
  �@      `�  �   �
  �@      t�  �   �
  A      ��  �   �
  @A          �   �
  �A      ��  $  v  Ț  ���                       B     
                    � ߱        ��    �  �   �      0B      4   ����0B      /   �  L�     \�                          3   ����@B            |�                      3   ����`B  �    �  ��  $�  �  |B      4   ����|B                4�                      ��                  �  >                  ���                       �  ��  H�  �   �  �B      ��  $  �  t�  ���                       C     
                    � ߱        ��  �   �  (C      �  $   �  ��  ���                       PC  @         <C              � ߱        ȝ  $  �  8�  ���                       �C       	       	           � ߱        D     
                �D                     �E  @        
 �E              � ߱        X�  V   �  d�  ���                        �E       	       	       $F       
       
       `F       	       	           � ߱        �  $  �  ��  ���                        G     
                �G                     �H  @        
 �H              � ߱        x�  V   �  ��  ���                        �H     
                tI                     �J  @        
 �J              � ߱            V   "  �  ���                                      ؠ                      ��                  @  �                  t�                       @  ��  �J     
                TK                     �L  @        
 dL          M  @        
 �L          lM  @        
 ,M          �M  @        
 �M              � ߱            V   U   �  ���                        adm-clone-props ��  �              �     7     `                          \  �                     start-super-proc    �  p�  �           �     8                                  �                     x�    �  ��  �      XQ      4   ����XQ      /   �  8�     H�                          3   ����hQ            h�                      3   �����Q  Т  $    ��  ���                       �Q                         � ߱        ��       �  h�  �  �Q      4   �����Q                ܣ                      ��                  !  %                  �	�                       !  ��  �Q                     �Q                      R                         � ߱            $  "  x�  ���                             &  $�  `�      R      4   ����R  8R                         � ߱            $  '  4�  ���                       ��    .  ��  ��  �  LR      4   ����LR      $  /  �  ���                       lR                         � ߱            �   L  �R      �R     
                <S                     �T  @        
 LT              � ߱        ��  V   `  $�  ���                        ȥ  �   �  �T      `�      �  ��      �T      4   �����T      /      �     0�                          3   �����T            P�                      3   ����U  �  $    ��  ���                       $U                         � ߱        PU     
                �U                     W  @        
 �V              � ߱        H�  V   $  ��  ���                        (�    �  d�  �      (W      4   ����(W                �                      ��                  �  �                  ���                       �  t�      g   �  �         p�̩                           Ш          ��  ��      ��                  �      ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     �  PW                      3   ����8W  <�     
   ,�                      3   ����\W         
   \�                      3   ����dW    ��                              ��        \                  ����                                        �              9      l�                      g                               0�  g   �  @�          p�	ԫ                           �          ت  ��      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  4�     D�  �W                      3   ����lW            d�                      3   �����W    ��                              ��        \                  ����                                        T�              :      t�                      g                               8�  g   �  H�          p�	ܭ                           �          �  Ȭ      ��                  �  �  ��              d��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  <�     L�  �W                      3   �����W            l�                      3   �����W    ��                              ��        \                  ����                                        \�              ;      |�                      g                               ��    �  T�  Ю      �W      4   �����W                �                      ��                  �  �                   ��                       �  d�  L�  /   �  �     �                          3   �����W            <�                      3   ����X  H�  /  �  x�     ��  XX                      3   ����8X  ��     
   ��                      3   ����`X  �        د                      3   ����hX  �        �                      3   ����|X            8�                      3   �����X  p�    �  d�  t�      �X      4   �����X      /  �  ��     ��  LY                      3   ����,Y  �     
   а                      3   ����TY  �         �                      3   ����\Y  @�        0�                      3   ����pY            `�                      3   �����Y        �  ��  ��      �Y      4   �����Y      /  �  ȱ     ر  Z                      3   �����Y  �     
   ��                      3   ����Z  8�        (�                      3   ����Z  h�        X�                      3   ����,Z            ��                      3   ����HZ  0�     �  lZ                                     �Z     
                �Z                     L\  @        
 \              � ߱        ��  V   \  ̲  ���                        `\     
                �\                     ,^  @        
 �]              � ߱        4�  V   �  \�  ���                        T^  @         @^          |^  @         h^              � ߱        `�  $   �  �  ���                       �  g   �  x�         p6��                            @�          �  ��      ��                  �  �  (�              ��-                    O   ����    e�          O   ����    R�          O   ����    ��            �  �^  }        ��                              ��        \                  ����                                        ��              <      X�                      g                               �  g   �  ,�         p"��                           ��          Ķ  ��      ��                  �  �  ܶ              <�-                    O   ����    e�          O   ����    R�          O   ����    ��          $  �   �  ���                       �^                         � ߱          ��                              ��        \                  ����                                        @�              =      L�                      g                               �  g   �   �         p"��                           �          ��  ��      ��                 �  �  и              H2/                    O   ����    e�          O   ����    R�          O   ����    ��            �  �  ��      �^      4   �����^                ��                      ��                  �  �                  �2/                       �  �  ̹  /   �  ��                                 3   �����^        �  �  ��  ��  �^      4   �����^      $  �  $�  ���                       _                         � ߱                      ̺                      ��                  �  �                  @3/                       �  P�  $�  $  �  ��  ���                       _                         � ߱            /   �  P�                                 3   ����$_    ��                              ��        \                  ����                                        4�              >      `�                      g                               l�      8�  ��      @_      4   ����@_                ļ                      ��                                      �ʒ                         H�  �  	    ��                                        3   ����T_  D�  /     4�                                 3   �����_  T�  �     �_      O     ��  ��  �_  �      ��  ��      �_      4   �����_      $     Ľ  ���                       T`  @         @`              � ߱        ��  /     �                                 3   ����\`                ؾ          ��  ��      ��                   #                  �ٓ                H�       ,�      O       ��          O       ��      �  /   !  �                                 3   ����x`      k   "  0�                    ��        �       /   &  t�                                 3   �����`  adm-create-objects  ��  ��                      ?      �                               �                     Clave-Administrador ��  ��              d	     @     �	                          �	  �                     disable_UI  �  d�                      A      �                               �  
                   enable_UI   p�  ��                      B      �                              �  	                   initializeObject    ��  4�                      C      $                              �                     Log-de-control  H�  ��              �     D                                 -                       �    ��      ���  �           X�  8   ����   h�  8   ����         x�  8   ����   ��  8   ����   ��  8   ����   ��  8   ����       8   ����       8   ����       ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��   �  �      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  \�  h�      returnFocus ,INPUT hTarget HANDLE   L�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  @�  P�      removeAllLinks  ,   0�  d�  t�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE T�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  X�  d�      hideObject  ,   H�  x�  ��      exitObject  ,   h�  ��  ��      editInstanceProperties  ,   ��  ��  ��      displayLinks    ,   ��  ��  ��      createControls  ,   ��  �  �      changeCursor    ,INPUT pcCursor CHARACTER   ��  H�  T�      applyEntry  ,INPUT pcField CHARACTER    8�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER p�  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  L�  T�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE <�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  ��  ��      startServerObject   ,   ��  �  �      runServerObject ,INPUT phAppService HANDLE  ��  D�  X�      restartServerObject ,   4�  l�  ��      initializeServerObject  ,   \�  ��  ��      disconnectObject    ,   ��  ��  ��      destroyServerObject ,   ��  ��  ��      bindServer  ,   ��  �  �      processAction   ,INPUT pcAction CHARACTER   ��  D�  T�      enableObject    ,   4�  h�  x�      disableObject   ,   X�  ��  ��      applyLayout ,   |�  ��  ��      viewPage    ,INPUT piPageNum INTEGER    ��  ��  ��      viewObject  ,   ��  �  �      toolbar ,INPUT pcValue CHARACTER    ��  8�  D�      selectPage  ,INPUT piPageNum INTEGER    (�  p�  ��      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER `�  ��  ��      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  ��  �   �      notifyPage  ,INPUT pcProc CHARACTER �  H�  T�      initPages   ,INPUT pcPageList CHARACTER 8�  ��  ��      initializeVisualContainer   ,   p�  ��  ��      hidePage    ,INPUT piPageNum INTEGER    ��  ��  ��      destroyObject   ,   ��  �  �      deletePage  ,INPUT piPageNum INTEGER    ��  D�  T�      createObjects   ,   4�  h�  x�      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE X�  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  (�  4�      changePage  ,   �  H�  \�      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 1%     adecomm/as-utils.w 
"   
   �    }        �
"     
   � �     "     �&    &        "      "  	        "    1� �    �%               "     �&    &         %                  "  9    "  9  �"     �"    �&    &    &    &        %              %                    *        "  ;    � �      %               "  ;    "     �"    �&    &    &    &        %              %               *    %                   "      +  %                   "      � !     � #  	   %              %              %              %                  
�    
�        �     }         �     }        �     }             �     }        �%                  �     }         �     }        �     }             �     }        �%              � 
"    
 �%              � �  �         `      $              
�    � �   �     
�             �G                      
�            � �   �
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 ��           �    1� �  
 �� �   �%               o%   o           � �    �
"   
 ��           D    1� �   �� �   �%               o%   o           � �   �
"   
 ��           �    1� �  
 �� �   �%               o%   o           � �   �
"   
 ��           ,    1� �   �� �   �%               o%   o           �   
 �
"   
 ��           �    1�    �� �   �%               o%   o           �    �
"   
 ��               1� 6   �� B   �%               o%   o           %               
"   
 ��          �    1� J   �� Z     
"   
 ��           �    1� a   �� �   �%               o%   o           � t  e �
"   
 ��           @	    1� �   �� �   �%               o%   o           � �  ? �
"   
 ��           �	    1� )   �� B   �%               o%   o           %               
"   
 ��           0
    1� 9   �� B   �%               o%   o           %               
"   
 ��           �
    1� K   �� B   �%               o%   o           %              
"   
 ��          (    1� X   �� B     
"   
 ��           d    1� g  
 �� B   �%               o%   o           %               
"   
 ��           �    1� r   �� �   �%               o%   o           � �    �
"   
 ��          T    1� z   �� Z     
"   
 ��           �    1� �   �� �   �%               o%   o           � �  t �
"   
 ��              1�   
 �� Z     
"   
 ��           @    1�     �� �   �%               o%   o           � 1  � �
"   
 ��           �    1� �   �� �   �%               o%   o           � �    �
"   
 ��           (    1� �  
 �� �   �%               o%   o           %               
"   
 ��           �    1� �   �� B   �%               o%   o           %               
"   
 ��                1� �   �� �   �%               o%   o           � �    �
"   
 ��           �    1� �   �� �   �%               o%   o           o%   o           
"   
 ��               1�   
 �� �   �%               o%   o           � �    �
"   
 ��           �    1�    �� )  	 �%               o%   o           � 3  / �
"   
 ��          �    1� c   �� )  	   
"   
 ��           4    1� u   �� )  	 �o%   o           o%   o           � �    �
"   
 ��          �    1� �   �� )  	   
"   
 ��           �    1� �   �� )  	 �o%   o           o%   o           � �    �
"   
 ��          X    1� �   �� B     
"   
 ��          �    1� �   �� )  	   
"   
 ��          �    1� �   �� )  	   
"   
 ��              1� �   �� )  	   
"   
 ��           H    1� �   �� B   �o%   o           o%   o           %              
"   
 ��          �    1� �   �� )  	   
"   
 ��               1� �  
 ��      
"   
 ��          <    1�    �� )  	   
"   
 ��          x    1�    �� )  	   
"   
 ��          �    1� 1   �� )  	   
"   
 ��          �    1� F   �� )  	   
"   
 ��          ,    1� U  	 �� )  	   
"   
 ��          h    1� _   �� )  	   
"   
 ��          �    1� r   �� )  	   
"   
 ��           �    1� �   �� �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
 �(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�       �    �� �     p�               �L
�    %              � 8      �    � $         � �          
�    � �     
"   
 �� @  , 
�       �    �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 ��           �    1� �  
 �� �   �%               o%   o           � �    �
"   
 ��           �    1� �  
 �� �   �%               o%   o           o%   o           
"   
 ��           x    1� �   �� Z   �%               o%   o           o%   o           
"   
 ��           �    1� �   �� B   �%               o%   o           %               
"   
 ��           p    1� �   �� B   �%               o%   o           %               
"   
 1�           �    1� �   1� �   �%               o%   o           � �    �
"   
 ��           `    1�    �� B   �%               o%   o           %              
"   
 ��           �    1�    �� B   �%               o%   o           o%   o           
"   
 ��           X    1� "   �� �   �%               o%   o           o%   o           
"   
 ��           �    1� 0  	 �� �   �%               o%   o           � �    �
"   
 ��           H    1� :   �� �   �%               o%   o           o%   o           
"   
 ��           �    1� N   �� �   �%               o%   o           o%   o           
"   
 ��           @    1� ]   �� B   �%               o%   o           %               
"   
 ��           �    1� m   �� B   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 ��           �    1� y   �� )  	 �%               o%   o           � �    �
"   
 ��                 1� �   �� )  	 �%               o%   o           � �    �
"   
 ��           t     1� �   �� B   �%               o%   o           %               
"   
 1�           �     1� �   1� )  	 �%               o%   o           � �    �
"   
 ��           d!    1� �   �� )  	 �%               o%   o           � �    1
"   
 ��           �!    1� �   �� B   �%               o%   o           %               
"   
 ��           T"    1� �   �� )  	 �%               o%   o           � �    �
"   
 ��           �"    1� �   �� )  	 �%               o%   o           � �    �
"   
 ��           <#    1� �   �� )  	 �%               o%   o           � �    �
"   
 ��           �#    1� �   �� )  	 �%               o%   o           o%   o           
"   
 ��           ,$    1�    �� )  	 �%               o%   o           � �    �
"   
 1�           �$    1�    1� )  	 �%               o%   o           � �    �
"   
 ��           %    1� %  	 ��    �%               o%   o           %               
"   
 ��           �%    1� /   ��    �%               o%   o           %               
"   
 ��           &    1� 8   �� B   �%               o%   o           o%   o           
"   
 ��           �&    1� I   �� B   �%               o%   o           o%   o           
"   
 ��           '    1� X   �� B   �%               o%   o           %               
"   
 ��           �'    1� f   �� B   �%               o%   o           %               
"   
 ��           �'    1� w   �� B   �%               o%   o           %               
"   
 1�           x(    1� �   1� �   �%               o%   o           %       
       
"   
 1�           �(    1� �   1� �   �%               o%   o           o%   o           
"   
 ��           p)    1� �   �� �   �%               o%   o           %              
"   
 ��           �)    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           h*    1� �   �� �   �%               o%   o           %              
"   
 ��           �*    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           `+    1� �   �� �   �%               o%   o           %              
"   
 ��           �+    1� �   �� �   �%               o%   o           o%   o           
"   
 1�           X,    1� �   1� )  	 �%               o%   o           � �    �P �L 
�H T   %              �     }        �GG %              
"   
 ��            -    1�     �� �   �%               o%   o           %               
"   
 ��           �-    1�    �� �   �%               o%   o           o%   o           
"   
 ��           .    1�    �� �   �%               o%   o           � �    �
"   
 ��           �.    1� (   �� �   �%               o%   o           � >  - �
"   
 ��            /    1� l   �� �   �%               o%   o           � �    �
"   
 ��           t/    1� �   �� �   �%               o%   o           � �   �
"   
 ��          �/    1� �   �� Z     
"   
 ��           $0    1� �   �� �   �%               o%   o           � �    �
"   
 ��          �0    1� �  
 �� Z     
"   
 ��          �0    1� �   �� Z     
"   
 ��           1    1� �   �� )  	 �%               o%   o           � �    �
"   
 ��           �1    1�     �� �   �%               o%   o           � �    �
"   
 ��           �1    1�    �� Z   �%               o%   o           o%   o           
"   
 ��           t2    1�    �� �   �%               o%   o           � -  ! �
"   
 ��           �2    1� O   �� �   �%               o%   o           � �    �
"   
 1�           \3    1� \   1� �   �%               o%   o           � o   �
"   
 1�           �3    1� ~  	 1� �   �%               o%   o           o%   o           
"   
 ��           L4    1� �   �� B   �%               o%   o           %               
"   
 ��          �4    1� �   �� Z     
"   
 ��           5    1� �   �� �   �%               o%   o           � �   �
"   
 ��           x5    1� �   �� )  	 �%               o%   o           � �    �
"   
 ��           �5    1� �   �� )  	 �%               o%   o           � �    �
"   
 ��          `6    1� �   �� Z     
"   
 ��          �6    1� �   �� )  	   
"   
 1�           �6    1�    1� B   �o%   o           o%   o           %               
"   
 ��          T7    1�    �� B     
"   
 ��          �7    1� 5   �� )  	   
"   
 ��          �7    1� C   �� )  	   
"   
 ��          8    1� V   �� )  	   
"   
 ��          D8    1� g   �� )  	   
"   
 ��          �8    1� x   �� )  	   
"   
 ��          �8    1� �   �� Z     
"   
 ��           �8    1� �   �� �   �%               o%   o           � �  4 �
"   
 ��          l9    1� �   �� Z     
"   
 ��          �9    1� �   �� Z     
"   
 ��          �9    1�    �� Z     
"   
 ��           :    1�    �� )  	   
"   
 ��          \:    1� $   �� )  	   
"   
 ��          �:    1� 6   �� )  	   
"   
 ��          �:    1� H   �� B     
"   
 ��           ;    1� U   �� )  	 �%               o%   o           � �    �
"   
 ��           �;    1� c   �� )  	 �%               o%   o           � �    �
"   
 ��           �;    1� o   �� )  	 �%               o%   o           � �    �
"   
 ��           l<    1� �   �� )  	 �%               o%   o           � �    �
"   
 ��           �<    1� �   �� B   �%               o%   o           %               
"   
 ��           \=    1� �   �� B   �%               o%   o           o%   o           
"   
 ��           �=    1� �   �� B   �%               o%   o           %               
"   
 ��           T>    1� �   �� B   �%               o%   o           %               
"   
 ��           �>    1� �   �� B   �%               o%   o           o%   o           
"   
 ��           L?    1� �   �� B   �%               o%   o           %               
"   
 ��          �?    1� �   �� )  	   
"   
 ��           @    1�    �� B   �%               o%   o           %              
"   
 ��          �@    1�    �� )  	   
"   
 ��          �@    1� )   �� )  	   
"   
 ��          �@    1� 8  
 �� )  	   
"   
 ��           4A    1� C   �� )  	 �%               o%   o           � �   �
"   
 ��           �A    1� U   �� )  	 �%               o%   o           � �    �
�             �G "    �%     start-super-proc ��%     adm2/smart.p q�P �L 
�H T   %              �     }        �GG %              
"   
   �       �B    6� �     
"   
   
�        �B    8
"   
   �        C    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        dD    �� �   � P   �        pD    �@    
� @  , 
�       |D    �� �   �p�               �L
�    %              � 8      �D    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �E    �� a   �p�               �L"  	  , �   � �   �� �   ��     }        �A      |    "  	    � �   �%              (<   \ (    |    �     }        �A� �   �A"  
  �    "  	  �"  
  �  < "  	  �"  
  �(    |    �     }        �A� �   �A"  
  �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        lG    �� �   � P   �        xG    �@    
� @  , 
�       �G    �� �   �p�               �L
�    %              � 8      �G    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �H    �� �  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 1(�  L ( l       �        DI    �� �   � P   �        PI    �@    
� @  , 
�       \I    �� �   �p�               �L
�    %              � 8      hI    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       xJ    �� J   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        $K    �� �   � P   �        0K    �@    
� @  , 
�       <K    �� �     p�               �L
�    %              � 8      HK    � $         � �          
�    � �     
"   
 �p� @  , 
�       XL    �� �  
 �p�               �L%     SmartDialog 
"   
   p� @  , 
�       �L    �� �     p�               �L% 
    DIALOG-BOX  
"   
  p� @  , 
�        M    �� �    p�               �L%               
"   
  p� @  , 
�       �M    �� u    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
 �    �        `N    �� �   �
"   
   � 8      �N    � $         � �          
�    � �   �
"   
   �        O    �
"   
   �       $O    /
"   
   
"   
   �       PO    6� �     
"   
   
�        |O    8
"   
   �        �O    �
"   
   �       �O    �
"   
   p�    � �   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �P    �A"    �A
"   
   
�        �P    �@ � 
"   
 �"      �       }        �
"   
 �%              %                "    �%     start-super-proc ��%     adm2/appserver.p Y��    � @     
�    �     }        �%               %      Server  - �     }        �    "    �� �    �%                   "    �� �    �%      NONE    p�,  8         $     "    1        � Z   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        S    �� �   � P   �        S    �@    
� @  , 
�       $S    �� �   �p�               �L
�    %              � 8      0S    � $         � �          
�    � �   �
"   
 �p� @  , 
�       @T    �� :   �p�               �L"    , p�,  8         $     "    1        � h   �
�     "    �%     start-super-proc ��%     adm2/visual.p ��   � �     � �     � �     
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �U    �� �   � P   �        �U    �@    
� @  , 
�       �U    �� �   �p�               �L
�    %              � 8      �U    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �V    �� �   �p�               �L"    , � 
" 	   
 �%     contextHelp 
" 	   
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP p�%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc �%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%      initializeDataObjects �0 0   A    �    � �   �
�    � �   �A    �    � �     
�    �    �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%     buildDataRequest ent0 A    �    � �   �
�    � !   �%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target %               
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        �Z    �� �   � P   �        �Z    �@    
� @  , 
�       �Z    �� �   �p�               �L
�    %              � 8      �Z    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�        \    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        �\    �� �   � P   �        �\    �@    
� @  , 
�       �\    �� �   �p�               �L
�    %              � 8      �\    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       �]    �� �   �p�               �L%              �             I%               �             �%              % 	    END-ERROR �� #  	       "    �� #  	 �%     Clave-Administrador     �  � #  	 �� #  	   � �     %     Log-de-Control  �     }        � `     @     ,         � f  (   G %       
       � �  &   G %       
       � �  & �% 
    disable_UI 
�    %                0   � 
�        
�             � 
%   
           
�             � 
�    %     createObjects   %     initializeObject �%     destroyObject   "     �"    �&    &    &    &        %              %              *    � #  	   � #  	   � *     � .         "      � �      � 2         "      � �      � *         "      � �      � �      "    �� *     � .         "      "      � J     � #  	   � 2         "      "      � g      � #  	   � *         "      "      � �     � #  	   "  	  �"    �"      "      %      SUPER       "      � �     �             %               �         �      �     \      H                "      � 	          "      �       � 	          "      �       � 	          "      �       �       %     lib/logtabla    "      "      "                      �           �   l       ��                 M  q  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��        $  \  �   ���                       N     
                    � ߱              ]  (  �      lN      4   ����lN                �                      ��                  ^  p                  ��                       ^  8  �  �  _  �N            a  �  `      O      4   ����O                p                      ��                  b  o                  tb�                       b  �  �  o   c      ,                                 �  �   d  0O      �  �   e  \O      $  $  f  �  ���                       �O     
                    � ߱        8  �   g  �O      L  �   h  �O      `  �   k  �O          $   n  �  ���                       P  @         P              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               hc�                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       lP     
                    � ߱                  �  �                      ��                   �  �                  �4�                     �  4      4   �����P      $  �  �  ���                       �P     
                    � ߱        �    �  4  D      �P      4   �����P      /  �  p                               3   ���� Q  �  �   �  Q          O   �  ��  ��  DQ                               , �                          
                               �      ��                            ����                                                        �   l       ��                  0  7  �               hړ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l           
               =  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��      �  A  �           ��           �`                                        �`   �`                   h  \           �`  �`           �`  �`         �            4   H                  �                      ��                  �  �                  4�                       �  |        �    �      a      4   ����a                �          �  �      ��                 �  �                  �l�                       �  $      O   �     ��  $a      O   �     ��  0a  �  p   �  <a  �      �  @  �     Ha        �    (      Ta      4   ����Ta      O   �  �� ��      �  P     ta        �  l  |      �a      4   �����a      O   �  �� ��          �     �a        �  �  �      �a      4   �����a      O   �  �� ��      l  $  �    ���                       �a                         � ߱        �a                         � ߱        �  ]   �  @    �                            p   �  �a  �      �  D  0     �a                @                      ��                  �  �                  �k�                       �  �        �  \  �      �a      4   �����a                �                      ��                  �  �                  Pl�                       �  l  ,  	  �                                          3   ����b      O   �  ��  ��  (b  �  �     4b                �                      ��                  �  �                  �l�                       �  T        �  �  h      @b      4   ����@b                x                      ��                  �  �                  Hm�                       �  �  �  	  �  �                                        3   ����`b      O   �  ��  ��  lb      P     xb                `                      ��                  �  �                  �m�                       �  �        �  |  �      �b      4   �����b                	                      ��                  �  �                  �r�                       �  �  L	  	  �  <	                                        3   �����b      O   �  ��  ��  �b               �	          �	  �	   , �	               I/C                                      I/C                                     ����                                                  �           �   l       ��                  �  �  �               �s�                    O   ����    e�          O   ����    R�          O   ����    ��             �  �� �                   ��                              ��        \                  ����                                            �           �   l       ��                  �  �  �               0��                    O   ����    e�          O   ����    R�          O   ����    ��      �b  �           �b  �              � ߱        @  Z   �  �    �                            �              �              � ߱        l  h   �     �                            
   �  �� �                  ��                              ��        \                  ����                                            �           �   l       ��                  �    �               ��                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       �b       	       	           � ߱        d  $  �  8  ���                       �b                         � ߱        �  /      �                                3   �����b          �  �       c      4   ���� c      $     �  ���                       4c  @          c              � ߱          ��                              ��        \                  ����                                            �           �   l       ��                      �               ��                    O   ����    e�          O   ����    R�          O   ����    ��        $    �   ���                       Hc                         � ߱        d  $    8  ���                       Tc                         � ߱        �  $    �  ���                       d                         � ߱            /     �     �                          3   ���� d  (                              3   ����<d  X        H                      3   ����Hd            x                      3   ����Td                         �  �   @ �                                                              0              0           ��                            ����                                �   @ m	d     �   ��  �  � �       6  �                                       �                                                               
         D                                                                 P   v� Jd                                                           �  E   
 X   v� _d                                                          �     D       D                                                                                        i    d d        ��  �  � �       d  �                                  \   	                                                         
   d     D                                                                 p  � � 8�         �  �                                                 ?     y                     P   HV�d                                                           <   G   
 X  HVxd         �                                          N     ~  
    \  � -�s                                 8                  W                 A      \  l-�s                                 -                  e                 B       D                                                                                            TXS appSrvUtils ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST pCodCli pNroCard pRpta x-CodUnico s-codcia cl-codcia OK FacCfgGn Configuracion General  gn-clie Maestro de Clientes gn-cliem Mensajes por cliente S ADM-ERROR Btn_Cancel Btn_OK EDITOR_Mensaje FILL-IN_ImpMn gDialog AVISO IMPORTANTE !!! x(8) ->>,>>9.99 DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDialog ContainerType DIALOG-BOX PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   Btn_OK Btn_Cancel CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target END-ERROR iStartPage A SmartDialog is not intended to be run  Persistent or to be placed in another  SmartObject at AppBuilder design time. ADM-CREATE-OBJECTS s-coddoc I/C clave gn-docpssw Clave por Tipo de Documento DEL ADD UPD Ingrese Clave x(20) CLAVE DE CREACION INCORRECTA CLAVE DE MODIFICACION INCORRECTA CLAVE DE ELIMINACION INCORRECTA Clave CLAVE-ADMINISTRADOR DISABLE_UI ENABLE_UI N INITIALIZEOBJECT cTabla cLlave cEvento gn-cliem | >>>9 >>>>>>>>>.99 VENTA MOSTRADOR LOG-DE-CONTROL MONTO MAXIMO A RETENER S/. CONTINUAR >>> <<< REGRESAR Llave01 idx01 Indice01 llave01 �  �      T&      & �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
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
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   Q	  i	  k	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props \  ]  ^  _  a  b  c  d  e  f  g  h  k  n  o  p  q              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  �  �  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                   �  �  T	  �	     =                                   �  �  �	  �	  
   >                                   �  �  �  �  �  �  �  �  �  �  �	  `
     ?               L
                  adm-create-objects  7  �
        x
     s-coddoc              �
     clave   
  �
  !   @   d
          �
                  Clave-Administrador �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �
  �     A               �                  disable_UI  �  �  h  �     B               �                  enable_UI   �  �  �  �  �  <     C               (                  initializeObject    �  �         l        d     cTabla  �        �     cLlave            �     cEvento �  �     D   P          �                  Log-de-control            �  �       (     �                      H          <  
   appSrvUtils h       \     x-CodUnico  �        |     s-codcia    �        �     cl-codcia   �    	   �     EDITOR_Mensaje  �       �     FILL-IN_ImpMn             
   gshAstraAppserver   @  	 	     ,  
   gshSessionManager   d  
 
     T  
   gshRIManager    �        x  
   gshSecurityManager  �        �  
   gshProfileManager   �        �  
   gshRepositoryManager            �  
   gshTranslationManager   0           
   gshWebManager   T        D     gscSessionId    x        h     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager   �        �  
   gshAgnManager           �     gsdTempUniqueID (             gsdUserObj  P        <     gsdRenderTypeObj    x        d     gsdSessionScopeObj  �       �  
   ghProp  �       �  
   ghADMProps  �       �  
   ghADMPropsBuf           �     glADMLoadFromRepos              glADMOk <       0  
   ghContainer \    	   P     cObjectName x    
   p     iStart  �       �     cAppService �       �     cASDivision �       �     cServerOperatingMode            �     cFields               iStartPage  @       8        pCodCli d       X        pNroCard             |        pRpta   �       �  FacCfgGn    �       �  gn-clie �       �  gn-cliem             �  gn-docpssw           <   �   �   �   �   �   �   �   �           
      �  �  �  �  �  �  �       !  "  9  E  F  G  I  K  L  M  Q  R  U  V  W  X  Z  \  ^  `  a  b  e  g  h  j  k  l  m  n  t  v  |  ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  
  
   
  !
  "
  #
  $
  %
  &
  '
  (
  )
  *
  +
  ,
  -
  .
  /
  0
  1
  2
  3
  4
  5
  6
  7
  8
  9
  :
  ;
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
  j  v  �  �  �  �  �  �  �  �  �  �  �  �  "  >  @  U  �  �  �       !  "  %  &  '  .  /  L  `  �        $  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  \  �  �  �  �  �                      !  "  #  &      
� % d:\newsie\on_in_co\APLIC\adm\i-DocPssw.i    �� $ C:\Progress\OpenEdge\src\adm2\dialogmn.i T  f!  C:\Progress\OpenEdge\src\adm2\containr.i �  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\visual.i      # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  4  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    t  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �  Ds   C:\Progress\OpenEdge\gui\fn  $  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   L  Q.  C:\Progress\OpenEdge\gui\set �  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i �  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    ,  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  p  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i �  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i $  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    X  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �  �j  C:\Progress\OpenEdge\gui\get   �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    <  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i ,  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   l  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  ,  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  `  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i �  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i      e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   X  �~   d:\newsie\on_in_co\APLIC\ccb\d-msgblo.w      4  �      �     H  %   �    (      �     �  $   �  �   �         �   �           {     $   �   v     4      T     D   �   L     T      �  #   d   �   �     t      �      �   �   �     �      �      �   �   �     �      �      �   r   �     �   n   �     �      C  "   �   i   >     !          !  P        $!  �   �     4!     �  !   D!  �   �     T!     {     d!  �   z     t!     X     �!  �   V     �!     4     �!  g        �!     �     �!  O   �     �!  �   m     �!     k      �!  �   ;     "     �     "  �   �     $"     �     4"  �   �     D"     �     T"  �   �     d"     p     t"  �   o     �"     M     �"  �   <     �"          �"  �        �"     �     �"  }   �     �"     �     �"     K     #     �     #     �     $#  7   s     4#  �   j     D#  O   \     T#     K     d#     �
     t#  �   �
     �#  �   �
     �#  O   �
     �#     �
     �#     ?
     �#  �   
     �#  x   
  
   �#  M   �	     �#     �	     $     �	     $  a   �	  
   $$  �  h	     4$     I	     D$  �  	     T$  O   	     d$     �     t$     �     �$  �   �     �$     �     �$     �     �$  x   �     �$     �     �$     d     �$     `     �$     L     %     3     %  Q   #  
   $%     �     4%     �  
   D%     }     T%     c  
   d%  f   8     t%     �  	   �%  "   �     �%          �%     ^     �%  Z        �%          �%     �     �%     �     �%     �     &     r     &  ,   �       $&     E      4&  	   "       D&     	      