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
/// This module represents the parsing of the Scribble Tool's output.
module BasicFSM =
    open CommonFSM

    type EventType = 
        | Send
        | Receive
                
    type Event =
        {
            Current     : Id
            Role        : Role
            Partner     : Partner
            Label       : Label
            Payload     : Payload     
            EventType   : EventType
            Next        : Id
        }

    type StateMachine = Events of Event list


/// This module is a translation of the previous module that is exisiting after the parsing part.
/// The goal of the types defined in this module are to provide a better represention of the session
/// with the introduction of Branching and Selection (and more in the future).
/// What should be outputed to the implementer of the ScribbleTP is an InitialState, that's it.
/// This module represents the CFSM that will be used during the whole type generation part.
module CFSM =
    open CommonFSM

    /// This types represent Session Types as defined in 
    /// http://mrg.doc.ic.ac.uk/publications/a-gentle-introduction-to-multiparty-asynchronous-session-types/paper.pdf
    /// Page 11. However we define a subset of them (for some reason related to how a CFSM is defined compared to 
    /// its homologue Session Type. Recursion for instance is not defined here but is rather implicit in CFSMs)
    type SessionType = 
        | Send of Transition
        | Receive of Transition
        | Branch of Transitions
        | Select of Transitions
        | End

    and Transitions    = Transitions of Transition list
    /// A transition doesn't contain the Action to perform. The transition only contains standalone information.
    /// Because the `States` type is a mapping `Id -> SessionTypes`, and SessionTypes only contain Transitions ( plus the meaning of the transitions)
    /// We can logically see this `Transition type` as a mapping `StartState -> EndState` and the arrow being some `Event` to apply to get therw
    and Transition = 
        {
            Role : Role
            Partner : Partner
            Label : Label
            Payload : Payload
            NextState : Id
        }
    type States = States of Map<Id,SessionType>
    /// The State type now contain the action to perform which itself knows which transition(s) it is linked to.
    type CFSM = 
        {
            FirstState : Id
            States : States
        }


/// This module provides Helper to translate a BasicFSM -> CFSM
module ConversionFSM =
    open CommonFSM
    open BasicFSM
    open CFSM
    open Error

    /// Represents all the possible Errors for the ConversionFSM module
    type ConversionFSMError =
    | NotIdenticalEventTypes
    | DuplicationOfTransition of Id*Id
    | MultipleTransition of Id*Id
    | InputStateNotUnique of Id list
    | ImpossibleEndStateNotYetDefined
    | CFSMWithoutAnyChannel
        interface IFailure with
            member this.Reason =
                match this with
                | NotIdenticalEventTypes -> 
                    "{NotIdenticalEventTypes} ->\n" +
                    "A list of transactions associated to a state, contains at least an event of type SEND and an event of type RECEIVE which can't happen"
                | DuplicationOfTransition (Id startId ,Id endId) ->
                    "{DuplicationOfTransition} ->\n" +
                    sprintf "Multiple Identical transition from state (%A) -> (%A) :" startId endId +
                    "Either Scribble returns multiple times the same channel, or there is an issue in the parsing of the scribble Output"
                | MultipleTransition (Id startId ,Id endId) ->
                    "{MultipleTransition} ->\n" +
                    sprintf "Multiple transition from state (%A) -> (%A) with different information : Issue in the Protocol Specification" startId endId
                | InputStateNotUnique ids ->
                    "{InputStateNotUnique} ->\n" +
                    sprintf "Expected a unique input state but either got None or strictly more than 1 => (%A) " ids
                | ImpossibleEndStateNotYetDefined ->
                    "{ImpossibleEndStateNotYetDefined} ->\n" +
                    "Got a End SessionType element when transforming a BasicFSM to a CFSM which should not be the case yet!!"
                | CFSMWithoutAnyChannel ->
                    "{CFSMWithoutAnyChannel} ->\n" +
                    "Got a CFSM with no transition whatsoever, meaning just 1 state and no communication with any participant!!"

                |> sprintf "(ConversionFSMError) :: %s"
            member this.Kind = Kind.ConversionError
    
    /// Allows to get the FirstState of the CFSM, and verifies that it is unique.
    let getFirstState (states:States) =
        let (States map) = states
        let allStates = map |> Map.toList |> List.map fst
        let sessionTypeList = map |> Map.toList |> List.map snd

        let rec aux (ids: Id list) (firstStates: Id list) =
            match ids with
            | [] -> 
                if firstStates.Length <> 1 then
                    raiseFailure (InputStateNotUnique firstStates)
                else
                    firstStates.Head
            | hd::tl ->
                let listOfSimilarNextState =
                    [   
                        for sessionType in sessionTypeList do
                            match sessionType with
                            | Send transition | Receive transition ->
                                if transition.NextState = hd then
                                    yield transition.NextState
                            | Branch (Transitions transitions) | Select (Transitions transitions) ->
                                yield!
                                    [
                                        for transition in transitions do
                                            if transition.NextState = hd then
                                                yield transition.NextState
                                    ]
                            | End -> ()
                    ]            
                if listOfSimilarNextState.IsEmpty then
                    aux tl (hd::firstStates)
                else
                    aux tl firstStates
                
        aux allStates []

    /// Update the CFSM, after being transformed from a BasicCFSM, by adding 
    /// End States, which are states that will be used to close the session with
    /// other participants
    let updateWithEndStatesIDs (cfsm:CFSM) = 
        let (States map) = cfsm.States
        let keys = map |> Map.toList |> List.map fst
        let transitions = 
            let values = map |> Map.toList |> List.map snd
            [
                for sessionType in values do
                    match sessionType with
                    | Send transition | Receive transition -> yield transition
                    | Select (Transitions transitions) | Branch (Transitions transitions) -> yield! transitions
                    | End -> ()
            ]
        
        // Get all the states that are the EndState of a transition but the StartState of none.
        let endStatesIDs = 
            [   
                for transition in transitions do
                    if not (List.contains (transition.NextState) keys) then
                        yield transition.NextState
            ]

        let mapWithEndStates =
            let mutable tmpMap = map
            for endStateId in endStatesIDs do
                tmpMap <- tmpMap.Add(endStateId,End)
            tmpMap
        {
            FirstState = cfsm.FirstState
            States = States mapWithEndStates
        }


    /// This is the main function that traverse the BasicFSM and transforms it to a CFSM
    let traverseStateMachine (initialState:StateMachine) =
        let (Events listOfEvents) = initialState

        let addingEventToExistingID 
            currentId transition oldTransitions 
            (map:Map<Id,SessionType>) (session:Transitions -> SessionType)=
            let _ =
                [
                    for oldTransition in oldTransitions do
                        if oldTransition = transition then
                            raiseFailure (DuplicationOfTransition (currentId,transition.NextState))
                        elif oldTransition.NextState = transition.NextState then
                            raiseFailure (MultipleTransition (currentId,transition.NextState))
                        else
                            yield oldTransition
                ]
                            
            map.Add(currentId, transition::oldTransitions |> Transitions |> session)

        let rec aux (events:Event list) (accStates:States) =
            match events with
            | [] -> 
                {   FirstState = getFirstState accStates
                    States = accStates }
            | event::tl ->
                let (States map) = accStates
                let id = event.Current
                let eventType = event.EventType
                let transition =
                    {   Role = event.Role
                        Partner = event.Partner
                        Label = event.Label
                        Payload = event.Payload
                        NextState = event.Next
                    }
                let newMap =
                    match map.TryFind id with
                    // State Not added yet
                    | None -> 
                        let sessionType = 
                            match eventType with
                            | EventType.Send -> Send transition
                            | EventType.Receive -> Receive transition
                        map.Add(id,sessionType)
                    // State already exists. 
                    // This means that normally we have either a branching strategy or a Select one. 
                    | Some session ->
                        match session,eventType with
                        | Send oldTransition , EventType.Send ->
                            addingEventToExistingID id transition [oldTransition] map Select

                        | Receive oldTransition , EventType.Receive ->
                            addingEventToExistingID id transition [oldTransition] map Branch

                        | Select (Transitions oldTransitions) , EventType.Send ->
                            addingEventToExistingID id transition oldTransitions map Select

                        | Branch (Transitions oldTransitions) , EventType.Receive ->
                            addingEventToExistingID id transition oldTransitions map Branch

                        | End , _ -> raiseFailure ImpossibleEndStateNotYetDefined
                        | _ , _ -> raiseFailure NotIdenticalEventTypes
                                
                aux tl (States newMap)  

        // we check we have at list one transition. If we do not then it means we do not have any communication with any participants.
        if listOfEvents.Length >= 1 then
            let cfsm = aux listOfEvents (States Map.empty<Id,SessionType>)
            updateWithEndStatesIDs cfsm
        else
            raiseFailure CFSMWithoutAnyChannel


