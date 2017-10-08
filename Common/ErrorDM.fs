namespace Common

open FSharp.Quotations
open FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations

// TODO : Maybe change in the future to Error handling for runtime
/// This module will contain so far our Error handling support at compile-Time, 
/// which will be based on raising particular exceptions.
module Error =
    
    /// This DU is incremented while the implementation is growing and there are 
    /// new kind of errors at compile-time that needs to be thrown/Compared...
    /// In order to give more context
    type Kind =
        | ConversionError

    type IFailure = 
        abstract member Reason : string
        abstract member Kind : Kind

    let raiseFailure (failure:IFailure) =   
        failwith "[Kind : %A] => %s" (failure.Kind) (failure.Reason)
