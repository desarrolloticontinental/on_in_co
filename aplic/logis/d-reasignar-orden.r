	��V��/b85  ��                                              � 3538010Butf-8 MAIN D:\newsie\on_in_co\aplic\logis\d-reasignar-orden.w,,INPUT pCodPHR CHARACTER,INPUT pNroPHR CHARACTER,INPUT pCodOrden CHARACTER,INPUT pNroOrden CHARACTER PROCEDURE Reasignar-Orden,, PROCEDURE initializeObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER      �%              ��              j� �%  (�              (f              �%    +   �T �  7   �Y `  8   �\ �   >   �] 8  ?   _ `  @   xc    A   xe �  B           r \  `u �  ? \y !  iSO8859-1                                                                           �$     �                                       �              �  4�                %  p"    �"   ��    �  4%         ��  �   d%      p%          �                                             PROGRESS                         �           
    
                    <              �                                                                                                     
          �             �                 �             �                                �         *                                �             �                                                                                          �             4                                                                                                        �                           �                                                                                          �                          INTEGRAL                         PROGRESS                         8     �  �      �   C                      ��_            �  2W                              �  �                      d  �  �g     CODCIACODPEDNROPEDFCHPEDFCHVENCODCLINOMCLIDIRCLICODDIVLUGENTLUGENT2FCHENTSEDECODSEDRUCCLINROORDCODALMCODMONTPOCMBUSUARIOUSRAPROBACIONDNICLIUSRANUUSRMODUSRDSCTOFCHCREFCHANUFCHMODFCHDSCTOOBSERVAFLGIGVIMPBRTIMPEXOIMPIGVIMPDTOIMPTOTSDOACTIMPISCIMPVTAIMPFLEIMPINTIMPCTOPORDTOPORIGVPESMATMRGUTICODORINROORIFLGESTFLGSITFCHSITNROCARDFMAPGONROREFCODREFCODDEPTCODPROVCODDISTCODPOSFLGENVCMPBNTETPOLICCODVENHORALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02GLOSACODTERIMPORTEFCHAPROBACIONFECSACHORSACUBIGEOUSRSACDIVDESUSRIMPODFCHIMPODUSRACTFECACTHORACTMOTREPOSICIONVTAPUNTUALORDCMPFLGIMPODZONAPICKEOUSRSACASIGNUSRSACRECEPUSUARIOINICIOUSUARIOFINITEMSPESOVOLUMENZONADISTRIBUCIONFCHFINFCHINICIOEMPAQESPEC                                                                      	          
                                                                                                                                                                             "          "          "          "       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6   "       7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O         P          Q          R          S         T          U          V          W   "       X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h   "       i   "       j          �     <  �      <                         % �]            E  i�                              �  �                      T  �  �H     CODCIACODPROCODDOCNRODOCFCHDOCFLGESTUSUARIOCODRUTDESRUTNOMTRACODCOBTPOTRACODMONCODVEHCTORUTFCHRETHORSALHORRETOBSERVCODDIVKMTINIKMTFINRESPONSABLEAYUDANTE-1AYUDANTE-2FCHSALGUIATRANSPORTISTAUSRCIERREFCHCIERRELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_D03LIBRE_D04LIBRE_D05LIBRE_L01LIBRE_L02LIBRE_L03LIBRE_L04LIBRE_L05LIBRE_F01LIBRE_F02LIBRE_F03LIBRE_F04LIBRE_F05FCHAPROBACIONGLOSAAPROBACIONUSRAPROBACIONAYUDANTE-4AYUDANTE-5AYUDANTE-6AYUDANTE-7TIPAYUDANTE-1TIPAYUDANTE-2TIPAYUDANTE-3TIPAYUDANTE-4TIPAYUDANTE-5TIPAYUDANTE-6TIPAYUDANTE-7AYUDANTE-3NROREFCODREFFECHAAPERTURAHORAAPERTURAUSERAPERTURAGLOSAAPERTURATIPRESPONSABLEFLGCIE                                                                         	          
                                                                                                                                                                                                                             !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          `  q
      �  
    
                  �  �             L                                                                                          q
          
    �
      �  
    
                  t  <  	           �                                                                                          �
          
  �  �
      4  
    
                     �  
           �                                                                                          �
          
  d  �
      �  
    
                  �  �             P                                                                                          �
          
    �
      �  
    
                  x  @             �                                                                                          �
          
  �  �
      8  
    
                  $  �             �                                                                                          �
          
  h  �
      �  
    
                  �  �             T                                                                                          �
          
    �
      �  
    
                  |  D                                                                                                        �
          
  �         <                         (  �             �                                                                                                       l        �                        �  �             X                                                                                                              �  
    
                  �  H                                                                                                                 
  �  )      @  
    
                  ,  �             �                                                                                          )          
  p  7      �  
    
                  �  �             \                                                                                          7          
    E      �                        �  L                                                                                                       E            �  U      D                        0  �             �                                                                                          U            t  `      �                        �  �             `                                                                                          `                q      �                        �                                                                                                          q                  ,   �      ,                         % �]            <   �E                              �  �                      8  �  �      CODCIACODDIVCODDOCNRODOCFECHAHORAUSUARIOEVENTOLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_C06LIBRE_C07LIBRE_C08LIBRE_C09LIBRE_C10                                                                      	          
                                                                                                           i   �      i                          �ɺ[            r   �                              �  �                       P!  �   �      CODCIACODDOCNRODOCCODREFNROREFFLGESTCODDIVHORATEGLOSAIMPCOBMONCOBHORESTHORLLEHORPARFLGESTDETLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02                                                                         	          
                                                                                                                                                                             	 ��                                              3 ��           $  H$  H p�"                            Seleccione la PHR destino!!! �                              
             
             
                                         
                                                                                                                H   X   �   �   �   �   �   �   �   �           0  @  P  `      H   X   �   �   �   �   �   �   �   �          0  @  P  `    ��                                                                              �          ����                            V    н  2                 �    �    �E    �    q�    �    =�    !   W
    undefined                                                               �       Խ  �   l   �                        �����               g�                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D            <          assignFocusedWidget         �      �     4       LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    H       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    Z       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          p       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    |       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    �       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |          LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �          LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    "      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 /      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    :      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    G      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    [      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    i      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    y      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    �      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    �      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
    �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER        u   ����  �             �   �           �   �          �   �             �            �              � ߱            Z   �����
   �p
                     H�    �  4  �      @      4   ����@                �                      ��                  �  �                  <�                       �  D  D    �  �  �      X      4   ����X      $  �    ���                       �  @         �              � ߱              �  `  p      �      4   �����      $  �  �  ���                         @         �              � ߱        assignPageProperty                              `  H      ��                  1  4  x              4��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            changePage                              �  �      ��                  6  7  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �  �      ��                  9  ;  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            constructObject                             �  �      ��                  =  B  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   @                            �� 
  h             4  
             ��   �             \               �� 
                 �  
         ��                            ����                            createObjects                               �  h      ��                  D  E  �              (X�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �  h      ��                  G  I  �              �X�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            destroyObject                               �  �      ��                  K  L  �              �\�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �  �      ��                  N  P  �              �_�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            initializeObject                                �  �      ��                  R  S  �              d�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               �  �      ��                  U  V                �d�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               �  �      ��                  X  Z                �g�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            notifyPage                                �      ��                  \  ^  ,              l�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  D           ��                            ����                            passThrough                             <  $      ��                  `  c  T              pp�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             l               ��                  �           ��                            ����                            removePageNTarget                               �  |      ��                  e  h  �              �x�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             �  
             ��                  �           ��                            ����                            selectPage                              �  �      ��                  j  l  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            toolbar                               �      ��                  n  p                 ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8           ��                            ����                            viewObject                              0          ��                  r  s  H               `��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                0!  !      ��                  u  w  H!              D��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `!           ��                            ����                            disablePagesInFolder    
      �!       "    �      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder �!      ,"      `"    �      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  @"      �"      �"    �      HANDLE, getCallerWindow �"      �"      �"          HANDLE, getContainerMode    �"       #      4#          CHARACTER,  getContainerTarget  #      @#      t#    (      CHARACTER,  getContainerTargetEvents    T#      �#      �#    ;      CHARACTER,  getCurrentPage  �#      �#      �#    T      INTEGER,    getDisabledAddModeTabs  �#      $      <$     c      CHARACTER,  getDynamicSDOProcedure  $      H$      �$  !  z      CHARACTER,  getFilterSource `$      �$      �$  "  �      HANDLE, getMultiInstanceActivated   �$      �$       %  #  �      LOGICAL,    getMultiInstanceSupported   �$      %      H%  $  �      LOGICAL,    getNavigationSource (%      T%      �%  %  �      CHARACTER,  getNavigationSourceEvents   h%      �%      �%  &  �      CHARACTER,  getNavigationTarget �%      �%      &  '        HANDLE, getOutMessageTarget �%      &      L&  (        HANDLE, getPageNTarget  ,&      T&      �&  )  +      CHARACTER,  getPageSource   d&      �&      �&  *  :      HANDLE, getPrimarySdoTarget �&      �&      �&  +  H      HANDLE, getReEnableDataLinks    �&      '      <'  ,  \      CHARACTER,  getRunDOOptions '      H'      x'  -  q      CHARACTER,  getRunMultiple  X'      �'      �'  .  �      LOGICAL,    getSavedContainerMode   �'      �'      �'  /  �      CHARACTER,  getSdoForeignFields �'      (      8(  0  �      CHARACTER,  getTopOnly  (      D(      p(  1 
 �      LOGICAL,    getUpdateSource P(      |(      �(  2  �      CHARACTER,  getUpdateTarget �(      �(      �(  3  �      CHARACTER,  getWaitForObject    �(      �(      ()  4  �      HANDLE, getWindowTitleViewer    )      0)      h)  5  �      HANDLE, getStatusArea   H)      p)      �)  6        LOGICAL,    pageNTargets    �)      �)      �)  7        CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �)      *      D*  8  &      LOGICAL,INPUT h HANDLE  setCallerProcedure  $*      \*      �*  9  6      LOGICAL,INPUT h HANDLE  setCallerWindow p*      �*      �*  :  I      LOGICAL,INPUT h HANDLE  setContainerMode    �*      �*      $+  ;  Y      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  +      L+      �+  <  j      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  `+      �+      �+  =  }      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �+      �+      (,  >  �      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  ,      X,      �,  ?  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource p,      �,      �,  @  �      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �,       -      4-  A  �      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   -      T-      �-  B  �      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   p-      �-      �-  C  �      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource �-      ,.      `.  D        LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   @.      �.      �.  E  %      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �.      �.      /  F  ?      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �.      8/      l/  G  S      LOGICAL,INPUT phObject HANDLE   setPageNTarget  L/      �/      �/  H  g      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �/      �/      0  I  v      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget �/      00      d0  J  �      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    D0      �0      �0  K  �      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �0      �0       1  L  �      LOGICAL,INPUT phObject HANDLE   setRunDOOptions  1      @1      p1  M  �      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  P1      �1      �1  N  �      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �1      �1       2  O  �      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields  2      L2      �2  P  �      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  `2      �2      �2  Q 
       LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �2      �2      (3  R        LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget 3      L3      |3  S  !      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    \3      �3      �3  T  1      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �3      �3      ,4  U  B      LOGICAL,INPUT phViewer HANDLE   getObjectType   4      L4      |4  V  W      CHARACTER,  setStatusArea   \4      �4      �4  W  e      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             l5  T5      ��                  �  �  �5              �º                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               p6  X6      ��                  �  �  �6              �ź                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                t7  \7      ��                  �  �  �7              �Ⱥ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                |8  d8      ��                  �  �  �8              |ɺ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �9  h9      ��                  �    �9              �̺                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �9           ��                            ����                            getAllFieldHandles  �4      :      L:  X  s      CHARACTER,  getAllFieldNames    ,:      X:      �:  Y  �      CHARACTER,  getCol  l:      �:      �:  Z  �      DECIMAL,    getDefaultLayout    �:      �:       ;  [  �      CHARACTER,  getDisableOnInit    �:      ;      @;  \  �      LOGICAL,    getEnabledObjFlds    ;      L;      �;  ]  �      CHARACTER,  getEnabledObjHdls   `;      �;      �;  ^  �      CHARACTER,  getHeight   �;      �;      �;  _ 	 �      DECIMAL,    getHideOnInit   �;      <      4<  `  �      LOGICAL,    getLayoutOptions    <      @<      t<  a  �      CHARACTER,  getLayoutVariable   T<      �<      �<  b        CHARACTER,  getObjectEnabled    �<      �<      �<  c        LOGICAL,    getObjectLayout �<       =      0=  d  0      CHARACTER,  getRow  =      <=      d=  e  @      DECIMAL,    getWidth    D=      p=      �=  f  G      DECIMAL,    getResizeHorizontal |=      �=      �=  g  P      LOGICAL,    getResizeVertical   �=      �=      >  h  d      LOGICAL,    setAllFieldHandles  �=      (>      \>  i  v      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    <>      |>      �>  j  �      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �>      �>      ?  k  �      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    �>      (?      \?  l  �      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   <?      |?      �?  m  �      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �?      �?       @  n  �      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout �?      $@      T@  o  �      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal 4@      x@      �@  p  �      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �@      �@      A  q  �      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �@      4A      hA  r  	      LOGICAL,    getObjectSecured    HA      tA      �A  s  %	      LOGICAL,    createUiEvents  �A      �A      �A  t  6	      LOGICAL,    bindServer                              �B  hB      ��                  �  �  �B              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �C  lC      ��                  �  �  �C              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �D  tD      ��                  �  �  �D              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �E  |E      ��                  �  �  �E              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �F  �F      ��                  �  �  �F              D�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �G  �G      ��                  �  �  �G              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �H  �H      ��                  �  �  �H              ���                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �H  
         ��                            ����                            startServerObject                               �I  �I      ��                  �  �  �I              X��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �J  �J      ��                  �  �  �J              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  K           ��                            ����                            getAppService   �A      xK      �K  u  E	      CHARACTER,  getASBound  �K      �K      �K  v 
 S	      LOGICAL,    getAsDivision   �K      �K      L  w  ^	      CHARACTER,  getASHandle �K      (L      TL  x  l	      HANDLE, getASHasStarted 4L      \L      �L  y  x	      LOGICAL,    getASInfo   lL      �L      �L  z 	 �	      CHARACTER,  getASInitializeOnRun    �L      �L      M  {  �	      LOGICAL,    getASUsePrompt  �L      M      DM  |  �	      LOGICAL,    getServerFileName   $M      PM      �M  }  �	      CHARACTER,  getServerOperatingMode  dM      �M      �M  ~  �	      CHARACTER,  runServerProcedure  �M      �M      N    �	      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �M      LN      |N  �  �	      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   \N      �N      �N  �   
      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �N      �N      $O  �  
      LOGICAL,INPUT phASHandle HANDLE setASInfo   O      DO      pO  � 	 
      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    PO      �O      �O  �  $
      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �O      �O      P  �  9
      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �O      <P      pP  �  H
      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  PP      �P      �P  �  Z
      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �Q  pQ      ��                  �  �  �Q              l3�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �Q             �Q  
             ��   R             �Q               �� 
                 R  
         ��                            ����                            addMessage                               S  �R      ��                  �  �  S              �A�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   dS             0S               ��   �S             XS               ��                  �S           ��                            ����                            adjustTabOrder                              |T  dT      ��                  �  �  �T              �H�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �T             �T  
             �� 
  U             �T  
             ��                  �T           ��                            ����                            applyEntry                              �U  �U      ��                  �  �  V              lP�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  $V           ��                            ����                            changeCursor                                 W  W      ��                  �  �  8W              �T�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  PW           ��                            ����                            createControls                              LX  4X      ��                  �  �  dX              4Y�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               PY  8Y      ��                  �  �  hY              �]�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                TZ  <Z      ��                  �  �  lZ              _�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              `[  H[      ��                  �  �  x[              �a�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              `\  H\      ��                  �  �  x\              �b�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              `]  H]      ��                  �  �  x]              �e�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                h^  P^      ��                  �  �  �^              @f�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              p_  X_      ��                  �  �  �_              �i�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �_             �_  
             ��   �_             �_               ��   $`             �_               ��                  `           ��                            ����                            modifyUserLinks                             a  �`      ��                  �  �  ,a              Xw�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   xa             Da               ��   �a             la               �� 
                 �a  
         ��                            ����                            removeAllLinks                              �b  xb      ��                  �  �  �b              �Z�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �c  xc      ��                       �c              [�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �c             �c  
             ��   d             �c               �� 
                 d  
         ��                            ����                            repositionObject                                e  �d      ��                    	  (e              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   te             @e               ��                  he           ��                            ����                            returnFocus                             `f  Hf      ��                      xf              ܎�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �f  
         ��                            ����                            showMessageProcedure                                �g  |g      ��                      �g              <��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �g             �g               ��                  �g           ��                            ����                            toggleData                              �h  �h      ��                      �h              p��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  i           ��                            ����                            viewObject                              j  �i      ��                      $j              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �P      |j      �j  � 
 �      LOGICAL,    assignLinkProperty  �j      �j      �j  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �j      @k      pk  �  �      CHARACTER,  getChildDataKey Pk      |k      �k  �  �      CHARACTER,  getContainerHandle  �k      �k      �k  �  �      HANDLE, getContainerHidden  �k      �k      (l  �        LOGICAL,    getContainerSource  l      4l      hl  �  !      HANDLE, getContainerSourceEvents    Hl      pl      �l  �  4      CHARACTER,  getContainerType    �l      �l      �l  �  M      CHARACTER,  getDataLinksEnabled �l      �l      ,m  �  ^      LOGICAL,    getDataSource   m      8m      hm  �  r      HANDLE, getDataSourceEvents Hm      pm      �m  �  �      CHARACTER,  getDataSourceNames  �m      �m      �m  �  �      CHARACTER,  getDataTarget   �m      �m       n  �  �      CHARACTER,  getDataTargetEvents  n      ,n      `n  �  �      CHARACTER,  getDBAware  @n      ln      �n  � 
 �      LOGICAL,    getDesignDataObject xn      �n      �n  �  �      CHARACTER,  getDynamicObject    �n      �n      o  �  �      LOGICAL,    getInstanceProperties   �n      $o      \o  �  �      CHARACTER,  getLogicalObjectName    <o      ho      �o  �        CHARACTER,  getLogicalVersion   �o      �o      �o  �  $      CHARACTER,  getObjectHidden �o      �o      p  �  6      LOGICAL,    getObjectInitialized    �o      (p      `p  �  F      LOGICAL,    getObjectName   @p      lp      �p  �  [      CHARACTER,  getObjectPage   |p      �p      �p  �  i      INTEGER,    getObjectParent �p      �p      q  �  w      HANDLE, getObjectVersion    �p      q      Pq  �  �      CHARACTER,  getObjectVersionNumber  0q      \q      �q  �  �      CHARACTER,  getParentDataKey    tq      �q      �q  �  �      CHARACTER,  getPassThroughLinks �q      �q      r  �  �      CHARACTER,  getPhysicalObjectName   �q       r      Xr  �  �      CHARACTER,  getPhysicalVersion  8r      dr      �r  �  �      CHARACTER,  getPropertyDialog   xr      �r      �r  �  �      CHARACTER,  getQueryObject  �r      �r      s  �        LOGICAL,    getRunAttribute �r       s      Ps  �        CHARACTER,  getSupportedLinks   0s      \s      �s  �  .      CHARACTER,  getTranslatableProperties   ps      �s      �s  �  @      CHARACTER,  getUIBMode  �s      �s      t  � 
 Z      CHARACTER,  getUserProperty �s      t      Lt  �  e      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    ,t      tt      �t  �  u      CHARACTER,INPUT pcPropList CHARACTER    linkHandles �t      �t       u  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    �t      $u      Tu  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry 4u      �u      �u  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �u      (v      Xv  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    8v      |v      �v  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  �v      �v      w  �  �      CHARACTER,  setChildDataKey �v      w      @w  �  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden   w      hw      �w  �  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  |w      �w      �w  �  �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    �w      x      Lx  �        LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled ,x      px      �x  �  (      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   �x      �x      �x  �  <      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents �x      y      Py  �  J      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  0y      xy      �y  �  ^      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   �y      �y      z  �  q      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents �y      (z      \z  �        LOGICAL,INPUT pcEvents CHARACTER    setDBAware  <z      �z      �z  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject �z      �z       {  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    �z      ({      \{  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   <{      x{      �{  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    �{      �{      |  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   �{      (|      \|  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   <|      �|      �|  �         LOGICAL,INPUT pcName CHARACTER  setObjectParent �|      �|       }  �        LOGICAL,INPUT phParent HANDLE   setObjectVersion    �|       }      T}  �        LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    4}      |}      �}  �  /      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks �}      �}      ~  �  @      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   �}      ,~      d~  �  T      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  D~      �~      �~  �  j      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �~      �~        �  }      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   �~      4      h  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   H      �      �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  �      �      �  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty �      8�      h�  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage H�      ��      Ԁ  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   ��      ��      $�  � 	 �      CHARACTER,INPUT pcName CHARACTER    �    /  d�  ��      @      4   ����@                ��                      ��                  0  ]                  �Ի                       0  t�        1  �  ��      P      4   ����P                ��                      ��                  2  \                  ջ                       2  �  ��    I  ��  0�      d      4   ����d                @�                      ��                  U  W                  �ջ                       U  Ă         V                                  @     
                    � ߱        ă  $  Y  l�  ���                           $  [  ��  ���                       �       	       	           � ߱        (�    a  8�  ��      �      4   �����                Ą                      ��                  b  &	                  @ֻ                       b  H�  ��  o   e      ,                                 P�  $   f  $�  ���                         @         �              � ߱        d�  �   g  0      x�  �   h  �      ��  �   j        ��  �   l  �      ��  �   n         ȅ  �   p  t      ܅  �   q  �      ��  �   r  ,      �  �   u  �      �  �   w        ,�  �   x  �      @�  �   z  	      T�  �   {  �	      h�  �   |  �	      |�  �   }  @
      ��  �   ~  �
      ��  �   �  �
      ��  �   �  d      ̆  �   �  �      ��  �   �        �  �   �  �      �  �   �        �  �   �  �      0�  �   �  �      D�  �   �  p      X�  �   �  �      l�  �   �  X      ��  �   �  �      ��  �   �        ��  �   �  D      ��  �   �  �      Ї  �   �  �      �  �   �  0      ��  �   �  l      �  �   �  �       �  �   �  $      4�  �   �  `      H�  �   �  �      \�  �   �  �      p�  �   �        ��  �   �  P      ��  �   �  �      ��  �   �  �      ��  �   �            �   �  @                      �          X�  @�      ��                  M	  {	  p�              T�                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                ,       
       
       <                         � ߱        �  $ a	  ��  ���                           O   y	  ��  ��  |               ��          t�  |�    d�                                             ��                            ����                                L4      Ԉ      0�     6     ��                      V ��  W                     �    �	  D�  ��      �      4   �����                Ћ                      ��                  �	  "
                  L�                       �	  T�  �  �   �	  �      ��  �   �	  \      �  �   �	  �       �  �   �	  T      4�  �   �	  �      H�  �   �	  L      \�  �   �	  �      p�  �   �	  <      ��  �   �	  �      ��  �   �	  4      ��  �   �	  �      ��  �   �	  $      Ԍ  �   �	  �          �   �	        ��    -
  �  ��      �      4   �����                ��                      ��                  .
  �
                  �	�                       .
  �  ��  �   0
  �      ��  �   1
  `      ̍  �   2
  �      ��  �   3
  P      �  �   4
  �      �  �   5
  8       �  �   6
  �       0�  �   7
  (!      D�  �   8
  �!      X�  �   9
  "      l�  �   :
  �"      ��  �   ;
   #      ��  �   <
  t#      ��  �   =
  �#      ��  �   >
  l$      Ў  �   ?
  �$      �  �   @
  d%      ��  �   A
  �%      �  �   B
  \&       �  �   C
  �&      4�  �   D
  T'      H�  �   E
  �'      \�  �   F
  L(      p�  �   G
  �(      ��  �   H
  D)      ��  �   I
  �)      ��  �   J
  <*          �   K
  �*      ܔ    �
  ܏  X�       +      4   ���� +                h�                      ��                  �
  z                  H�                       �
  �  |�  �   �
  �+      ��  �   �
  �+      ��  �   �
  x,      ��  �   �
  �,      ̐  �   �
  `-      ��  �   �
  �-      ��  �   �
  H.      �  �   �
  �.      �  �   �
  �.      0�  �   �
  4/      D�  �   �
  p/      X�  �   �
  �/      l�  �   �
  X0      ��  �   �
  �0      ��  �   �
  H1      ��  �   �
  �1      ��  �   �
  02      Б  �   �
  �2      �  �   �
  (3      ��  �   �
  d3      �  �   �
  �3       �  �   �
  L4      4�  �   �
  �4      H�  �   �
  �4      \�  �   �
  85      p�  �   �
  �5      ��  �   �
  �5      ��  �   �
  ,6      ��  �   �
  h6      ��  �   �
  �6      Ԓ  �   �
  �6      �  �   �
  7      ��  �   �
  X7      �  �   �
  �7      $�  �   �
  8      8�  �   �
  D8      L�  �   �
  �8      `�  �   �
  �8      t�  �   �
  �8      ��  �   �
  49      ��  �   �
  p9      ��  �   �
  �9      ē  �   �
  X:      ؓ  �   �
  �:      �  �   �
  @;       �  �   �
  �;      �  �   �
  8<      (�  �      �<      <�  �     0=      P�  �     �=      d�  �     (>      x�  �     d>      ��  �     �>      ��  �     ?      ��  �     X?      Ȕ  �     �?          �   	  @      4�  $  �  �  ���                       p@     
                    � ߱        ̕    �  P�  `�      �@      4   �����@      /   �  ��     ��                          3   �����@            ��                      3   �����@   �    �  �  d�  P�  �@      4   �����@  	              t�                      ��             	     �  N                  x�                       �  ��  ��  �   �  0A      ��  $  �  ��  ���                       \A     
                    � ߱        ��  �   �  |A      L�  $   �   �  ���                       �A  @         �A              � ߱        �  $  �  x�  ���                       �A                         � ߱        lB     
                �B       
       
       8D  @        
 �C              � ߱        ��  V   �  ��  ���                        DD                     xD                     �D                         � ߱        (�  $  �  4�  ���                       tE     
                �E       
       
       @G  @        
  G              � ߱        ��  V     Ę  ���                        LG     
                �G       
       
       I  @        
 �H              � ߱            V   2  T�  ���                        
              �                      ��             
     P  �                  \�                       P  �  ,I     
                �I       
       
       �J  @        
 �J          \K  @        
 K          �K  @        
 �K           L  @        
 �K              � ߱            V   e  `�  ���                        adm-clone-props ̊  D�              �     7     `                          \                       start-super-proc    T�  ��  �           �     8                                  3                     ��      <�  L�      �O      4   �����O      /     x�     ��                          3   �����O            ��                      3   �����O  �  $     �  ���                       �O                         � ߱        ̞    0  ,�  ��  H�  P      4   ����P                �                      ��                  1  5                  ��                       1  <�  ,P                     @P                     TP                         � ߱            $  2  ��  ���                             6  d�  ��      lP      4   ����lP  �P                         � ߱            $  7  t�  ���                       ȟ    >  �  ��  P�  �P      4   �����P      $  ?  $�  ���                       �P                         � ߱            �   \  �P      Q     
                �Q       
       
       �R  @        
 �R              � ߱        ��  V   p  d�  ���                        �  �   �  �R      ��    %  $�  4�      ,S      4   ����,S      /   &  `�     p�                          3   ����<S            ��                      3   ����\S  \�  $  *  ̠  ���                       xS                         � ߱        �S     
                 T       
       
       pU  @        
 0U              � ߱        ��  V   4  ��  ���                        h�    �  ��   �      |U      4   ����|U                0�                      ��                  �  �                  ���                       �  ��      g   �  H�         ���                           �          �  Ȣ      ��                  �      ��               ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  <�     L�  �U                      3   �����U  |�     
   l�                      3   �����U         
   ��                      3   �����U    ��                              ��        �                  ����                                        \�              9      ��                      g                               p�  g   �  ��          ��	�                           H�          �   �      ��                  �  �  0�              ���                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  t�     ��  �U                      3   �����U            ��                      3   �����U    ��                              ��        �                  ����                                        ��              :      ��                      g                               x�  g   �  ��          ��	�                           P�           �  �      ��                  �  �  8�              L��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  |�     ��  V                      3   ���� V            ��                      3   ����$V    ��                              ��        �                  ����                                        ��              ;      ��                      g                               ج    �  ��  �      @V      4   ����@V                 �                      ��                  �  �                  p��                       �  ��  ��  /   �  L�     \�                          3   ����PV            |�                      3   ����pV  ��  /  �  ��     ȩ  �V                      3   �����V  ��     
   �                      3   �����V  (�        �                      3   �����V  X�        H�                      3   �����V            x�                      3   �����V  ��    �  ��  ��      W      4   ����W      /  �  �     �  �W                      3   �����W   �     
   �                      3   �����W  P�        @�                      3   �����W  ��        p�                      3   �����W            ��                      3   �����W        �  ̫  ܫ      X      4   ����X      /  �  �     �  \X                      3   ����<X  H�     
   8�                      3   ����dX  x�        h�                      3   ����lX  ��        ��                      3   �����X            Ȭ                      3   �����X  p�     �  �X                                     �X     
                PY       
       
       �Z  @        
 `Z              � ߱         �  V   l  �  ���                        �Z     
                0[       
       
       �\  @        
 @\              � ߱        t�  V   �  ��  ���                        �\  @         �\          �\  @         �\              � ߱        ��  $   �  ,�  ���                       T�  g   �  ��         �6��                            ��          P�  8�      ��                  �  �  h�              P��                    O   ����    e�          O   ����    R�          O   ����    ��            �  �\  }        ��                              ��        �                  ����                                        ̮              <      ��                      g                               �  g     l�         �"��                           4�          �  �      ��                   #  �              쒾                    O   ����    e�          O   ����    R�          O   ����    ��      ��  $     `�  ���                       ]  @         �\              � ߱        ��      ��  $�      $]      4   ����$]                4�                      ��                    !                  d��                         ��  |�      P�  ̲  ��  ,]      4   ����,]                ܲ                      ��                                      Ѕ�                         `�      	    �                                        3   ����L]                ��                      ��                                      L��                          �   �  	    г                         �]        �  3   ����X]  �  3   �����]      3   �����]  d�  V     ,�  ���                                                    ߱                            ��  ��      �]      4   �����]                �                      ��                                      ���                         ��  d�  $     8�  ���                       ^  @         ^              � ߱            O    ������  0^  ��  /     ��                                 3   ����D^          Ե  P�      `^      4   ����`^                `�                      ��                                       ��                         �  ��  	    ��                                        3   ����x^      O    ������  �^      $   "  �  ���                       �^  @         �^              � ߱                      H�                                   �       ��                              ��        �                  ����                            ě          ��  �         =     P�                      g   L�                          \�    @  (�  ��      �^      4   �����^                ��                      ��                  @  H                  ���                       @  8�  ��  	  A  �                                        3   �����^  4�  /   E  $�                                 3   ����H_  D�  �   F  `_      O   G  ��  ��  h_  �    K  x�  ��      |_      4   ����|_      $   L  ��  ���                       �_  @         �_              � ߱        ��  /   N  �                                 3   �����_                Ⱥ          ��  ��      ��                 S  W                  ,��                8�     S  �      O   S    ��          O   S    ��      �  /   U  ��                                 3   �����_      k   V   �                    ��        �       /   Z  d�                                 3   ����`  adm-create-objects  ��  t�                      >      �                               �                     disable_UI  ��  �                      ?      �                               �  
                   enable_UI   �  L�                      @      �             <              �  	                   initializeObject    X�  ��                      A      �                                                    Reasignar-Orden ȼ  $�          �         B     �                          �  �                       �    ���   �  +  Seleccione la PHR destino!!!  ���  �           ��  8   ����   �  8   ����   �  8   ����   $�  8   ����   4�  8   ����   D�  8   ����       8   ����       8   ����       d�  p�      toggleData  ,INPUT plEnabled LOGICAL    T�  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  ��  �      returnFocus ,INPUT hTarget HANDLE   �  ,�  @�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    �  |�  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE l�  ܿ  �      removeAllLinks  ,   ̿   �  �      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE �  h�  |�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    X�  ��   �      hideObject  ,   ��  �   �      exitObject  ,   �  4�  L�      editInstanceProperties  ,   $�  `�  p�      displayLinks    ,   P�  ��  ��      createControls  ,   t�  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  �  ,�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER �  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER t�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  D�  T�      unbindServer    ,INPUT pcMode CHARACTER 4�  |�  ��      startServerObject   ,   l�  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  ��  ��      restartServerObject ,   ��  �   �      initializeServerObject  ,   ��  4�  H�      disconnectObject    ,   $�  \�  p�      destroyServerObject ,   L�  ��  ��      bindServer  ,   t�  ��  ��      processAction   ,INPUT pcAction CHARACTER   ��  ��  ��      enableObject    ,   ��  �  �      disableObject   ,   ��  (�  4�      applyLayout ,   �  H�  T�      viewPage    ,INPUT piPageNum INTEGER    8�  ��  ��      viewObject  ,   p�  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��  ��  ��      selectPage  ,INPUT piPageNum INTEGER    ��  �   �      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER ��  \�  h�      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  L�  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��  ��  ��      initPages   ,INPUT pcPageList CHARACTER ��  �  8�      initializeVisualContainer   ,   �  L�  X�      hidePage    ,INPUT piPageNum INTEGER    <�  ��  ��      destroyObject   ,   t�  ��  ��      deletePage  ,INPUT piPageNum INTEGER    ��  ��  ��      createObjects   ,   ��  �  �      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  x�  ��  ��      changePage  ,   ��  ��  ��      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 �%     adecomm/as-utils.w 
"   
   �    }        �
"     
   %              %              %              %       	       %              %              "      "      "      "      "      %       	       %                  
�    
�        �     }         �     }        �     }             �     }        �%                  �     }         �     }        �     }             �     }        �%              � 
"    
 �%              � ��  �         �      \     H     $              
�    � �   �     
�             �G� �   �G     
�             �G                      
�            � �     
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 ��           $    1� �  
 ��    �%               o%   o           �     �
"   
 ��           �    1�    ��    �%               o%   o           �    �
"   
 ��               1� "  
 ��    �%               o%   o           � -   �
"   
 ��           �    1� 9   ��    �%               o%   o           � G  
 �
"   
 ��           �    1� R   ��    �%               o%   o           � a   �
"   
 ��           h    1� x   �� �   �%               o%   o           %               
"   
 ��          �    1� �   �� �     
"   
 ��                1� �   ��    �%               o%   o           � �  e �
"   
 ��           �    1�    ��    �%               o%   o           � +  ? �
"   
 ��               1� k   �� �   �%               o%   o           %               
"   
 ��           �    1� {   �� �   �%               o%   o           %               
"   
 ��            	    1� �   �� �   �%               o%   o           %              
"   
 ��          |	    1� �   �� �     
"   
 ��           �	    1� �  
 �� �   �%               o%   o           %               
"   
 ��           4
    1� �   ��    �%               o%   o           �     �
"   
 ��          �
    1� �   �� �     
"   
 ��           �
    1� �   ��    �%               o%   o           � �  t �
"   
 ��          X    1� W  
 �� �     
"   
 ��           �    1� b   ��    �%               o%   o           � s  � �
"   
 ��               1�     ��    �%               o%   o           �     �
"   
 ��           |    1�   
 �� "   �%               o%   o           %               
"   
 ��           �    1� &   �� �   �%               o%   o           %               
"   
 ��           t    1� .   ��    �%               o%   o           �     �
"   
 ��           �    1� ?   ��    �%               o%   o           o%   o           
"   
 ��           d    1� O  
 ��    �%               o%   o           �     �
"   
 ��           �    1� Z   �� k  	 �%               o%   o           � u  / �
"   
 ��          L    1� �   �� k  	   
"   
 ��           �    1� �   �� k  	 �o%   o           o%   o           �     �
"   
 ��          �    1� �   �� k  	   
"   
 ��           8    1� �   �� k  	 �o%   o           o%   o           �     �
"   
 ��          �    1� �   �� �     
"   
 ��          �    1� �   �� k  	   
"   
 ��          $    1�    �� k  	   
"   
 ��          `    1�    �� k  	   
"   
 ��           �    1�    �� �   �o%   o           o%   o           %              
"   
 ��              1� 0   �� k  	   
"   
 ��          T    1� >  
 �� I     
"   
 ��          �    1� Q   �� k  	   
"   
 ��          �    1� `   �� k  	   
"   
 ��              1� s   �� k  	   
"   
 ��          D    1� �   �� k  	   
"   
 ��          �    1� �  	 �� k  	   
"   
 ��          �    1� �   �� k  	   
"   
 ��          �    1� �   �� k  	   
"   
 ��           4    1� �   ��    �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
 �(�  L ( l       �        �    �� �   � P   �            �@    
� @  , 
�           �� �     p�               �L
�    %              � 8           � $         � �          
�    �      
"   
 �� @  , 
�       0    �� "  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 ��           �    1�   
 ��    �%               o%   o           �     �
"   
 ��           P    1�   
 ��    �%               o%   o           o%   o           
"   
 ��           �    1�    �� �   �%               o%   o           o%   o           
"   
 ��           H    1� #   �� �   �%               o%   o           %               
"   
 ��           �    1� 2   �� �   �%               o%   o           %               
"   
 ��           @    1� ?   ��    �%               o%   o           �     �
"   
 ��           �    1� F   �� �   �%               o%   o           %              
"   
 ��           0    1� X   �� �   �%               o%   o           o%   o           
"   
 ��           �    1� d   ��    �%               o%   o           o%   o           
"   
 ��           (    1� r  	 ��    �%               o%   o           �     �
"   
 ��           �    1� |   ��    �%               o%   o           o%   o           
"   
 ��               1� �   ��    �%               o%   o           o%   o           
"   
 ��           �    1� �   �� �   �%               o%   o           %               
"   
 ��               1� �   �� �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 ��           �    1� �   �� k  	 �%               o%   o           �     �
"   
 ��           T    1� �   �� k  	 �%               o%   o           �     �
"   
 ��           �    1� �   �� �   �%               o%   o           %               
"   
 ��           D    1� �   �� k  	 �%               o%   o           �     �
"   
 ��           �    1� �   �� k  	 �%               o%   o           �     �
"   
 ��           ,     1�    �� �   �%               o%   o           %               
"   
 ��           �     1�    �� k  	 �%               o%   o           �     �
"   
 ��           !    1�    �� k  	 �%               o%   o           �     �
"   
 ��           �!    1� -   �� k  	 �%               o%   o           �     �
"   
 ��           "    1� ;   �� k  	 �%               o%   o           o%   o           
"   
 ��           �"    1� I   �� k  	 �%               o%   o           �     �
"   
 ��           �"    1� Y   �� k  	 �%               o%   o           �     �
"   
 ��           h#    1� g  	 �� I   �%               o%   o           %               
"   
 ��           �#    1� q   �� I   �%               o%   o           %               
"   
 ��           `$    1� z   �� �   �%               o%   o           o%   o           
"   
 ��           �$    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           X%    1� �   �� �   �%               o%   o           %               
"   
 ��           �%    1� �   �� �   �%               o%   o           %               
"   
 ��           P&    1� �   �� �   �%               o%   o           %               
"   
 ��           �&    1� �   �� �   �%               o%   o           %       
       
"   
 ��           H'    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           �'    1� �   �� �   �%               o%   o           %              
"   
 ��           @(    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           �(    1�    �� �   �%               o%   o           %              
"   
 ��           8)    1�    �� �   �%               o%   o           o%   o           
"   
 ��           �)    1�     �� �   �%               o%   o           %              
"   
 ��           0*    1� (   �� �   �%               o%   o           o%   o           
"   
 ��           �*    1� 0   �� k  	 �%               o%   o           �     �P �L 
�H T   %              �     }        �GG %              
"   
 ��           t+    1� B   �� "   �%               o%   o           %               
"   
 ��           �+    1� N   �� "   �%               o%   o           o%   o           
"   
 ��           l,    1� Z   ��    �%               o%   o           �     �
"   
 ��           �,    1� j   ��    �%               o%   o           � �  - �
"   
 ��           T-    1� �   ��    �%               o%   o           �     �
"   
 ��           �-    1� �   ��    �%               o%   o           � �   �
"   
 ��          <.    1�     �� �     
"   
 ��           x.    1�    ��    �%               o%   o           �     �
"   
 ��          �.    1�   
 �� �     
"   
 ��          (/    1� (   �� �     
"   
 ��           d/    1� 5   �� k  	 �%               o%   o           �     �
"   
 ��           �/    1� B   ��    �%               o%   o           �     �
"   
 ��           L0    1� O   �� �   �%               o%   o           o%   o           
"   
 ��           �0    1� \   ��    �%               o%   o           � o  ! �
"   
 ��           <1    1� �   ��    �%               o%   o           �     �
"   
 ��           �1    1� �   ��    �%               o%   o           � �   �
"   
 ��           $2    1� �  	 �� "   �%               o%   o           o%   o           
"   
 ��           �2    1� �   �� �   �%               o%   o           %               
"   
 ��          3    1� �   �� �     
"   
 ��           X3    1� �   ��    �%               o%   o           � �   �
"   
 ��           �3    1�    �� k  	 �%               o%   o           �     �
"   
 ��           @4    1�    �� k  	 �%               o%   o           �     �
"   
 ��          �4    1� $   �� �     
"   
 ��          �4    1� 6   �� k  	   
"   
 ��           ,5    1� I   �� �   �o%   o           o%   o           %               
"   
 ��          �5    1� `   �� �     
"   
 ��          �5    1� w   �� k  	   
"   
 ��           6    1� �   �� k  	   
"   
 ��          \6    1� �   �� k  	   
"   
 ��          �6    1� �   �� k  	   
"   
 ��          �6    1� �   �� k  	   
"   
 ��          7    1� �   �� �     
"   
 ��           L7    1� �   ��    �%               o%   o           � �  4 �
"   
 ��          �7    1� (   �� �     
"   
 ��          �7    1� 5   �� �     
"   
 ��          88    1� E   �� �     
"   
 ��          t8    1� R   �� k  	   
"   
 ��          �8    1� f   �� k  	   
"   
 ��          �8    1� x   �� k  	   
"   
 ��          (9    1� �   �� �     
"   
 ��           d9    1� �   �� k  	 �%               o%   o           �     �
"   
 ��           �9    1� �   �� k  	 �%               o%   o           �     �
"   
 ��           L:    1� �   �� k  	 �%               o%   o           �     �
"   
 ��           �:    1� �   �� k  	 �%               o%   o           �     �
"   
 ��           4;    1� �   �� �   �%               o%   o           %               
"   
 ��           �;    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           ,<    1� �   �� �   �%               o%   o           %               
"   
 ��           �<    1�    �� �   �%               o%   o           %               
"   
 ��           $=    1�    �� �   �%               o%   o           o%   o           
"   
 ��           �=    1� 2   �� �   �%               o%   o           %               
"   
 ��          >    1� @   �� k  	   
"   
 ��           X>    1� N   �� �   �%               o%   o           %              
"   
 ��          �>    1� _   �� k  	   
"   
 ��          ?    1� k   �� k  	   
"   
 ��          L?    1� z  
 �� k  	   
"   
 ��           �?    1� �   �� k  	 �%               o%   o           � �   �
"   
 ��           �?    1� �   �� k  	 �%               o%   o           �     �
�             �G "  	  �%     start-super-proc �%     adm2/smart.p ��P �L 
�H T   %              �     }        �GG %              
"   
   �       $A    6� �     
"   
   
�        PA    8
"   
   �        pA    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �B    �� �   � P   �        �B    �@    
� @  , 
�       �B    �� �   �p�               �L
�    %              � 8      �B    � $         � �          
�    �    �
"   
 �p� @  , 
�       �C    �� �   �p�               �L"    , �   � �   �� �   ��     }        �A      |    "      � �   �%              (<   \ (    |    �     }        �A� �   �A"    �    "    �"    �  < "    �"    �(    |    �     }        �A� �   �A"    �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �E    �� �   � P   �        �E    �@    
� @  , 
�       �E    �� �   �p�               �L
�    %              � 8      �E    � $         � �          
�    �    �
"   
 �p� @  , 
�       �F    �� �  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        �G    �� �   � P   �        �G    �@    
� @  , 
�       �G    �� �   �p�               �L
�    %              � 8      �G    � $         � �   �     
�    �    �
"   
 �p� @  , 
�       �H    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        xI    �� �   � P   �        �I    �@    
� @  , 
�       �I    �� �     p�               �L
�    %              � 8      �I    � $         � �          
�    �      
"   
 �p� @  , 
�       �J    �� "  
 �p�               �L%     SmartDialog 
"   
   p� @  , 
�       K    �� 9     p�               �L% 
    DIALOG-BOX  
"   
  p� @  , 
�       tK    �� �    p�               �L%               
"   
  p� @  , 
�       �K    �� �    p�               �L(        �       �       �       �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
 �    �        �L    �� �   �
"   
   � 8       M    � $         � �          
�    �    �
"   
   �        XM    �
"   
   �       xM    /
"   
   
"   
   �       �M    6� �     
"   
   
�        �M    8
"   
   �        �M    �
"   
   �       N    �
"   
   p�    �    �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �N    �A"    �A
"   
   
�         O    �@ � 
"   
 �"      �       }        �
"   
 �%              %                "  	  �%     start-super-proc �%     adm2/appserver.p ��    � �     
�    �     }        �%               %      Server  - �     }        �    "    ��     �%                   "    ��     �%      NONE    p�,  8         $     "    �        � �   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        `Q    �� �   � P   �        lQ    �@    
� @  , 
�       xQ    �� �   �p�               �L
�    %              � 8      �Q    � $         � �          
�    �    �
"   
 �p� @  , 
�       �R    �� |   �p�               �L"    , p�,  8         $     "    �        � �   �
�     "  	  �%     start-super-proc �%     adm2/visual.p ��   � �     � �     � �     
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �S    �� �   � P   �        �S    �@    
� @  , 
�       T    �� �   �p�               �L
�    %              � 8      T    � $         � �          
�    �    �
"   
 �p� @  , 
�       $U    ��    �p�               �L"    , � 
" 	   
 �%     contextHelp 
" 	   
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP ��%     processAction   
�    %     CTRL-PAGE-DOWN  "  	  �%     start-super-proc �%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%      initializeDataObjects �0 0   A    �    � 2   �
�    � D   �A    �    � 2     
�    � P   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%     buildDataRequest ent0 A    �    � 2   �
�    � m   �%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target %               
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �         Y    �� �   � P   �        ,Y    �@    
� @  , 
�       8Y    �� �   �p�               �L
�    %              � 8      DY    � $         � �   �     
�    �    �
"   
 �p� @  , 
�       TZ    �� $   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �         [    �� �   � P   �        [    �@    
� @  , 
�       [    �� �   �p�               �L
�    %              � 8      $[    � $         � �   �     
�    �    �
"   
 �p� @  , 
�       4\    �� �   �p�               �L%              �             I%               �             �%              % 	    END-ERROR ��            �%               *        "      "      � �  /    4               � �     "      �      "      %      
            �      "      "         "    �%               �            �%              %               %     Reasignar-Orden     �  �   	 �� "  )   %               �            �%              �     }        � `     @     ,         � W  (   G %       
       � �  &   G %       
       � �  & �% 
    disable_UI 
�    %                0   � 
�        
�             � 
%   
           
�             � 
�    %     createObjects   %     initializeObject �%     destroyObject   "    �"    �"    �� �   �� �   �%               "       "       &    &    &    &    &    &    d ,   @            "       &        "       &        "       &        S    "       &    &    "    �%      SUPER   �            B           "      �      "    ��            B           "      �      "    ��   	   �   	   � &     � *     "      "     "     &    &    &    &    &    &    p    L    0        %              %              %                  "      &        "  4    &    "  2    "  3    "      "       "       "      "      "       � T      +      C  � `      "      "      "  2    "  3    "     �"     �"    �"    �"    �"    �&    &    &    &    &    &    &    &    &    &    &    &    &    &    &    &    �    �    d    @            "      &        "      &        "     &        "      &        "      &        "      &    "      "       "       "      "      "       � T      +      C  � `      "      "      "      "      � �                      �           �   l       ��                 ]  �  �               |=�                    O   ����    e�          O   ����    R�          O   ����    ��        $  l  �   ���                       hL     
                    � ߱              m  (  �      �L      4   �����L                �                      ��                  n  �                  �E�                       n  8  �  �  o  M            q  �  `      dM      4   ����dM                p                      ��                  r                    �E�                       r  �  �  o   s      ,                                 �  �   t  �M      �  �   u  �M      $  $  v  �  ���                       �M     
                    � ߱        8  �   w  �M      L  �   x  N      `  �   {  <N          $   ~  �  ���                       lN  @         XN              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               (G�                    O   ����    e�          O   ����    R�          O   ����    ��      "                      �          �  $  �    ���                       �N     
                    � ߱                  �  �                      ��                   �  �                  hz�                     �  4      4   �����N      $  �  �  ���                       ,O     
                    � ߱        �    �  4  D      @O      4   ����@O      /  �  p                               3   ����TO  �  �   �  `O          O   �  ��  ��  �O                               , �                          
                               �      ��                            ����                                                        �   l       ��                  d  k  �               젾                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  q  |  �               죾                    O   ����    e�          O   ����    R�          O   ����    ��             {  �� �                   ��                              ��        �                  ����                                            �           �   l       ��                  �  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��      4`  �           @`  �          L`  �              � ߱        `  Z   �  �    �                            �              �              �              � ߱        �  h   �      �                        �  
   �  �� �                    s   �  �       �                        X  8                               7   ����           ��                �`   ��          �                  6   �         �   ��               �`   ��          �                                                                $             �`  �`  �`           �`  �`  �`                      �            J   �        ���    ��                                                         da                      �                 X`   d`   p`   �`   �`      ��                              ��        �                  ����                            V        2                 �                    �           �   l       ��                  �  �  �               Ԫ�                    O   ����    e�          O   ����    R�          O   ����    ��      �   /   �  �                                 3   ����pa  H  $   �    ���                       �a  @         �a              � ߱            $   �  t  ���                       �a  @         �a              � ߱          ��                              ��        �                  ����                                                       �   l       ��                 �  �  �               t��                    O   ����    e�          O   ����    R�          O   ����    ��                    �              0      ��                �  �  H              ���                x     �  �       O   �     ��  b      O   �     ��   b        �      �  |      t  \      ��      @          �  �  �              ܸ�                      �  `        X       ��                            7   ����        ��               �b    �            �                  6   �       �   ��         �  �b    �            �                                                        ,b   8b   Db   Pb   \b                 H  <           hb  xb  �b           pb  �b  �b         �               $        O   �    ��          O   ����  R�          O   ����  ��      �  $  �  �  ���                       ,c                         � ߱        �  $  �  (  ���                       8c                         � ߱        Dc       3       3           � ߱        �  V   �  T  ���                        �  9   �     Pc                     \c                     hc                     tc                     �c                     �c       	       	       �c                     �c                         � ߱        X  V   �  �  ���                        �c       
       
       �c                     �c                     �c                         � ߱        �  V   �  �  ���                            8  �                 d	  l      	  �      ��      @          �  �   	              �ƾ                       �  �      <  �       ��                            7   ����
       ����               �d    �            �                  6   �  
     ,  ����            �d    �            �                                                        �c   �c   �c   d   d    d                   �  �           ,d  <d  Ld  \d           4d  Dd  Td  dd           ld  |d  �d  �d           td  �d  �d  �d         � �     �   	 
        H   d  	 �  
 �        O   �    ��          O   ����  R�          O   ����  ��      |e                         � ߱        �	  V   �  8	  ���                        �
  9   �     �e                     �e                     �e                     �e                     �e                     �e       	       	       �e                     �e                         � ߱        <  V   �  �	  ���                        �e       
       
       �e                     f                     f                         � ߱        h  V   �  �
  ���                            8  �     �  8  �     �  8  �         O   �  ��  ��  f                �                                           ��                             ��                             ��                            ����                                =   �         =   �         �!"          V  �
   �X                              
 �                                                                 <  �     `  
     �                                     
 �                                                                <  �     k       K�                                     
 �                                                                <  �     p         �                                     
 �                                                                <  �     v         �                                     
 �                                                                <  �     {  <     -�                                       �                                                                                                                                       L    d d     �   ��0  �0  � �       �  �                                  �   �                                                        
   d     D                                                                
 X  �� �d          d   x           
                              �     �      H  �b�!"                                V          �           P   ,&��Q                                                           �   G   
 X  ,&��d         �   �           
                              "     �     
 X  �#�Td         �   �           
                   
           0     �      \  4��s                                 �                  �                 B      \  ���s                                 �                  �                 A      P ��#g�s           ,          
                              �        D                                                                                                        TXS appSrvUtils pCodPHR pNroPHR pCodOrden pNroOrden ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST s-codcia s-coddiv s-user-id x-vtacdocu VtaCDocu x-msg Btn_Cancel Btn_OK FILL-IN-1 Seleccione la PHR destino!!! FILL-IN-orden FILL-IN-phr DI-RutaC Cabecera de ruta BROWSE-18 99/99/9999 x(3) X(15) x(6) x(60) gDialog Reasignar Ordenes a una nueva PHR X(256) X(20) Origen de la Orden DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDialog ContainerType DIALOG-BOX PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   BROWSE-18 Btn_Cancel Btn_OK CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target END-ERROR x-rowid Las PHR deben ser difentes..Imposible reasignar rpta Seguro de reasignar la Orden  - hacia la nueva PHR  ADM-ERROR NO se complet� el proceso de reasignaci�n iStartPage A SmartDialog is not intended to be run  Persistent or to be placed in another  SmartObject at AppBuilder design time. ADM-CREATE-OBJECTS DISABLE_UI PHR PX,PK,PF ENABLE_UI INITIALIZEOBJECT x-CodOri x-nroori HPK A LogisLogControl Tabla Control Logistico PHR_REASIGN HH:MM:SS DI-RutaD Detalle de ruta OK REASIGNAR-ORDEN Generada FchDoc Cod.Doc CodDoc Numero NroDoc Division CodDiv Glosa Observ Orden Cancel Reasignar llave01 Llave12 Llave02 h  �      �%      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
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
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   a	  y	  {	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props l  m  n  o  q  r  s  t  u  v  w  x  {  ~    �  �              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  �  �  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                   �  �  �	        �	     x-rowid �	        �	     x-msg             �	     rpta    T	  
     =   �	                                                                 !  "  #  �	  �
     >               �
                  adm-create-objects  k  `
  �
     ?               �
                  disable_UI  {  |  �
  (     @                                 enable_UI   �  �  �  �  �  �
  �     A               l                  initializeObject    �  �  �  �  �        �     x-CodOri              �     x-nroori    <       B   �                             Reasignar-Orden �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �      �  �  P                      �          �  
   appSrvUtils �        �     s-codcia    �        �     s-coddiv                 s-user-id   4       ,     x-msg   T       H     FILL-IN-1   x       h     FILL-IN-orden   �       �     FILL-IN-phr �        �  
   gshAstraAppserver   �  	 	     �  
   gshSessionManager     
 
     �  
   gshRIManager    4           
   gshSecurityManager  \        H  
   gshProfileManager   �        p  
   gshRepositoryManager    �        �  
   gshTranslationManager   �        �  
   gshWebManager   �        �     gscSessionId                  gsdSessionObj   D        4  
   gshFinManager   h        X  
   gshGenManager   �        |  
   gshAgnManager   �        �     gsdTempUniqueID �        �     gsdUserObj  �        �     gsdRenderTypeObj                  gsdSessionScopeObj  <       4  
   ghProp  \       P  
   ghADMProps  �       p  
   ghADMPropsBuf   �    	   �     glADMLoadFromRepos  �    
   �     glADMOk �       �  
   ghContainer        �     cObjectName              iStart  @       4     cAppService `       T     cASDivision �       t     cServerOperatingMode    �       �     cFields          �     iStartPage  �       �        pCodPHR                 pNroPHR ,                pCodOrden            D        pNroOrden   l     C  `  x-vtacdocu  �       |  DI-RutaC    �      �  LogisLogControl          �  DI-RutaD             <   �  �  �  �  �  �  �  /  0  1  2  I  U  V  W  Y  [  \  ]  a  b  e  f  g  h  j  l  n  p  q  r  u  w  x  z  {  |  }  ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  &	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  "
  -
  .
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
  <
  =
  >
  ?
  @
  A
  B
  C
  D
  E
  F
  G
  H
  I
  J
  K
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
                     	  z  �  �  �  �  �  �  �  �  �  �  �  �    2  N  P  e  �         0  1  2  5  6  7  >  ?  \  p  �  %  &  *  4  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  l  �  �  �    @  A  E  F  G  H  K  L  N  S  U  V  W  Z      �� $ C:\Progress\OpenEdge\src\adm2\dialogmn.i �  f!  C:\Progress\OpenEdge\src\adm2\containr.i �  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i      ��  C:\Progress\OpenEdge\src\adm2\visual.i   `  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  �  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    �  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i     I�  C:\Progress\OpenEdge\src\adm2\smart.i    P  Ds   C:\Progress\OpenEdge\gui\fn  �  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �  Q.  C:\Progress\OpenEdge\gui\set �  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i   ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    H  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i   ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i D  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i �  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i @  �j  C:\Progress\OpenEdge\gui\get t  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i $  Su  C:\Progress\OpenEdge\src\adm2\globals.i  X  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �  �  C:\Progress\OpenEdge\src\adm2\appsprto.i   ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   D  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  �  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  �  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i   ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    8  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �  �L    D:\newsie\on_in_co\aplic\logis\d-reasignar-orden.w       H  \      0     2  $   @  �   �      P  �   �     `     �     p  �   �     �     d     �  �   \     �       #   �  �   �     �     �      �  �   �     �     �      �  �   �            �         r   �         n   �     0      S  "   @   i   N     P      ,     `   P        p   �   
     �      �  !   �   �   �     �      �     �   �   �     �      h     �   �   f     �      D     �   g   *      !          !  O   �      !  �   }     0!     {      @!  �   K     P!     �     `!  �   �     p!     �     �!  �   �     �!     �     �!  �   �     �!     �     �!  �        �!     ]     �!  �   L     �!     *      "  �   '     "           "  }   �     0"     �     @"     [     P"          `"     �     p"  7   �     �"  �   z     �"  O   l     �"     [     �"          �"  �   �
     �"  �   �
     �"  O   �
     �"     �
      #     O
     #  �   *
      #  x   "
  
   0#  M   
     @#     �	     P#     �	     `#  a   �	  
   p#  �  x	     �#     Y	     �#  �  &	     �#  O   	     �#     	     �#     �     �#  �   �     �#     �     �#     
      $  x        $     �      $     t     0$     p     @$     \     P$     C     `$  Q   3  
   p$     �     �$     �  
   �$     �     �$     s  
   �$  f   H     �$     �  	   �$  "   �     �$     �     �$     n      %  Z        %     %      %     �     0%     �     @%     �     P%     �     `%  /   �       p%     H      �%  	   "       �%     	      