namespace ScribbleGenerativeTypeProvider

/// This module contains common Types between different evolution of the CFSM definition
/// which should never change.
module CommonFSM =

    type Role = Role of string
    type Partner = Partner of string
    type Label = Label of string
    type Payload = Payload of string List
    type Id = Id of int


/// Communication Finite State Machine 
/// This module defines a part of the Sast Domain Model : the BasicFSM
/// We define here a state machine that is State driven and not transition driven 
/// as given by the Scribble tool.
/// This means that a State will be defined as a List of transitions to other States 
/// depending of inputs. Which compared to the transition model is easier to reason with.
/// What should be outputed to the implementer of the ScribbleTP is an InitialState, that's it.
/// This initialState contains the following states recursively up to the End State(s).
/// Same as CFSM module.
/// This module represents the parsing of the Scribble Tool's output.
module BasicFSM =
    open CommonFSM

    type EventType = 
        | Send
        | Receive
    
    type State =
        {
            Id : Id
            Transitions : Transitions        
        }
    and Transitions    = Transitions of Transition list
    /// This transition doesn't need any information about the current State,
    /// because it is defined as an oriented Transition from a specific state.
    and Transition  = 
        {
            Role : Role
            Partner : Partner
            Label : Label
            Payload : Payload
            EventType : EventType
            NextState : State
        }



/// This module is a translation of the previous module that is exisiting after the parsing part.
/// The goal of the types defined in this module are to provide a better represention of the session
/// with the introduction of Branching and Selection (and more in the future).
/// What should be outputed to the implementer of the ScribbleTP is an InitialState, that's it.
/// This initialState contains the following states recursively up to the End State(s).
/// Same as BasicFSM module.
/// This module represents the CFSM that will be used during the whole type generation part.
module CFSM =
    open CommonFSM

    /// This types represent Session Types as defined in 
    /// http://mrg.doc.ic.ac.uk/publications/a-gentle-introduction-to-multiparty-asynchronous-session-types/paper.pdf
    /// Page 11. However we define a subset of them (for some reason related to how a CFSM is defined compared to 
    /// its homologue Session Type : Recursion for instance is not defined here but is rather implicit in CFSMs)
    type SessionType = 
        | Send of Transition
        | Receive of Transition
        | Branch of Transitions
        | Select of Transitions
        | End
    /// The State type now contain the action to perform which itself knows which transition(s) it is linked to.
    and State =
        {
            Id : Id
            SessionType : SessionType
        }
    and Transitions    = Transitions of Transition list
    /// A transition doesn't contain the Action to perform. The transition only contains standalone information.
    and Transition = 
        {
            Role : Role
            Partner : Partner
            Label : Label
            Payload : Payload
            NextState : State
        }
