{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server part.

module Pos.Communication.Server
       ( allListeners
       , allStubListeners
       , serverLoggerName
       , module Pos.Communication.Server.SysStart
       ) where

import           Data.Tagged                       (untag)
import           Node                              (Listener, ListenerAction (..))
import           System.Wlog                       (LoggerName, WithLogger)
import           Universum

import           Pos.Binary.Communication          ()
import           Pos.Block.Network.Listeners       (blockListeners, blockStubListeners)
import           Pos.Communication.BiP             (BiP)
import           Pos.Communication.Server.Protocol (protocolListeners,
                                                    protocolStubListeners)
import           Pos.Communication.Server.SysStart
import           Pos.Communication.Util            (modifyListenerLogger)
import           Pos.Constants                     (networkReceiveTimeout)
import           Pos.Delegation.Listeners          (delegationListeners,
                                                    delegationStubListeners)
import           Pos.Ssc.Class.Listeners           (SscListenersClass (..))
import           Pos.Txp.Listeners                 (txListeners, txStubListeners)
import           Pos.Update                        (usListeners, usStubListeners)
import           Pos.Util                          (convWithTimeLimit,
                                                    sendActionsWithTimeLimit, withWaitLog,
                                                    withWaitLogConvL)
import           Pos.Util.TimeWarp                 (sec)
import           Pos.WorkMode                      (WorkMode)

-- | All listeners running on one node.
allListeners
    :: (SscListenersClass ssc, WorkMode ssc m)
    => [Listener BiP m]
allListeners =
    map addWaitLogging $
    map (addTimeout networkReceiveTimeout) $
    map (modifyListenerLogger serverLoggerName) $
    concat
        [ map (modifyListenerLogger "block") blockListeners
        , map (modifyListenerLogger "ssc") $ untag sscListeners
        , map (modifyListenerLogger "tx") txListeners
        , map (modifyListenerLogger "delegation") delegationListeners
        , map (modifyListenerLogger "protocol") protocolListeners
        , map (modifyListenerLogger "update") usListeners
        ]
  where
    addWaitLogging (ListenerActionOneMsg f) =
        ListenerActionOneMsg $ \nId sA msg -> f nId (withWaitLog sA) msg
    addWaitLogging (ListenerActionConversation f) =
        ListenerActionConversation $ \nId cA -> f nId (withWaitLogConvL nId cA)

    addTimeout timeout (ListenerActionOneMsg f) =
        ListenerActionOneMsg $ \nId sA msg ->
            f nId (sendActionsWithTimeLimit timeout sA) msg
    addTimeout timeout (ListenerActionConversation f) =
        ListenerActionConversation $ \nId cA ->
            f nId (convWithTimeLimit timeout nId cA)

-- | All listeners running on one node.
allStubListeners
    :: (SscListenersClass ssc, WithLogger m) => Proxy ssc -> [Listener BiP m]
allStubListeners p =
    concat
        [ blockStubListeners p
        , sscStubListeners p
        , txStubListeners p
        , delegationStubListeners
        , protocolStubListeners
        , usStubListeners
        ]

  where
    addWaitLogging (ListenerActionOneMsg f) =
        ListenerActionOneMsg $ \nId sA msg -> f nId (withWaitLog sA) msg
    addWaitLogging (ListenerActionConversation f) =
        ListenerActionConversation $ \nId cA -> f nId (withWaitLogConvL nId cA)

    addTimeout _       (ListenerActionOneMsg f) = ListenerActionOneMsg f
    addTimeout timeout (ListenerActionConversation f) =
        ListenerActionConversation $ \nId cA ->
            f nId (convWithTimeLimit timeout nId cA)


-- | Logger name for server.
serverLoggerName :: LoggerName
serverLoggerName = "server"
