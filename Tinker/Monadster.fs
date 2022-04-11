module Tinker.Monadster

module OldCommon =
    type Label = string
    type VitalForce = {units:int}

    // M stands for "Monadster part generator"
    type M<'LiveBodyPart> = M of (VitalForce -> 'LiveBodyPart * VitalForce)
    
    let getVitalForce vitalForce =
        let oneUnit = {units = 1}
        let remaining = {units = vitalForce.units-1}
        oneUnit, remaining
        
    let runM (M f) vitalForce = f vitalForce // unwrap and apply   
    
    let mapM f bodyPartM =
        let transformWhileAlive vitalForce =
            let bodyPart, remainingVitalForce = runM bodyPartM vitalForce
            let updatedBodyPart = f bodyPart
            updatedBodyPart, remainingVitalForce
        M transformWhileAlive
    
    let map2M f m1 m2 =
        let becomeAlive vitalForce =
            let v1, remainingVitalForce = runM m1 vitalForce
            let v2, remainingVitalForce2 = runM m2 remainingVitalForce
            let v3 = f v1 v2
            v3, remainingVitalForce2
        M becomeAlive
    
    let returnM x =
        let becomeAlive vitalForce = x, vitalForce
        M becomeAlive
    
    let bindM f bodyPartM = // f is a monadic function: something -> M<something_else>
        let becomeAlive vitalForce =
            let bodyPart, remainingVitalForce = runM bodyPartM vitalForce
            runM (f bodyPart) remainingVitalForce
        M becomeAlive
    
    let applyM mf mx =
        let becomeAlive vitalForce =
            let f, remainingVitalForce = runM mf vitalForce
            let x, remainingVitalForce2 = runM mx remainingVitalForce
            let y = f x
            y, remainingVitalForce2
        M becomeAlive 
    
    let (<*>) = applyM
    let (<!>) = mapM

module Common =
        type Label = string
        type VitalForce = {units:int}

        // M stands for "Monadster part generator"
        type M<'LiveBodyPart> = M of (VitalForce -> 'LiveBodyPart * VitalForce)
    
        let getVitalForce vitalForce =
            let oneUnit = {units = 1}
            let remaining = {units = vitalForce.units-1}
            oneUnit, remaining
        
        let returnM x =
            let becomeAlive vitalForce = x, vitalForce
            M becomeAlive

        let runM (M f) vitalForce = f vitalForce // unwrap and apply   
    
        let bindM f bodyPartM = // f is a monadic function: something -> M<something_else>
            let becomeAlive vitalForce =
                let bodyPart, remainingVitalForce = runM bodyPartM vitalForce
                runM (f bodyPart) remainingVitalForce
            M becomeAlive
            
        type MonsterBuilder() =
            member this.Return(x) = returnM x
            member this.Bind(xM,f) = bindM f xM
            
        let monster = MonsterBuilder()
        
        let mapM f xM = monster {
            let! x = xM
            return f x
        }
        
        let map2M f xM yM = monster {
            let! x = xM
            let! y = yM
            return f x y
        }
        
        let applyM fM xM = monster {
            let! f = fM
            let! x = xM
            return f x
        }
        
        let getM =
            let doSomethingWhileLive vitalForce =
                vitalForce, vitalForce
            M doSomethingWhileLive
            
        let putM newVitalForce =
            let doSomethingWhileLive _ =
                (), newVitalForce
            M doSomethingWhileLive
        
        (*
        let useUpOneUnitM =
            monster.Bind (getM, fun vf ->
                let u, r = getX vf 
                monster.Bind (putM r, fun () ->
                    monster.Return u))
        *)    
        let useUpOneUnitM = monster {
            let! vitalForce = getM
            let oneUnit, remainingVitalForce = getVitalForce vitalForce
            do! putM remainingVitalForce // this goes "in" the (return oneUnit)
            return oneUnit
        }
        
        let (<*>) = applyM
        let (<!>) = mapM

module LeftLeg =
    open Common
    type DeadLeftLeg = DeadLeftLeg of Label
    type LiveLeftLeg = LiveLeftLeg of Label * VitalForce
    type MakeLiveLeftLeg = DeadLeftLeg -> (VitalForce -> LiveLeftLeg * VitalForce)

    let makeLiveLeftLegM deadLeftLeg =
        let becomeAlive vitalForce =
            let (DeadLeftLeg label) = deadLeftLeg
            let oneUnit, remainingVitalForce = getVitalForce vitalForce
            let liveLeftLeg = LiveLeftLeg (label,oneUnit)
            liveLeftLeg, remainingVitalForce
        M becomeAlive
    
    let makeLiveLeftLegM2 deadLeftLeg = monster {
        let (DeadLeftLeg label) = deadLeftLeg
        let! oneUnit = useUpOneUnitM
        return LiveLeftLeg (label,oneUnit)
    }
    
    let test () =
        let deadLeftLeg = DeadLeftLeg "Boris"
        let leftLegM = makeLiveLeftLegM2 deadLeftLeg
        let vf = {units = 10}
        let liveLeftLeg, remainingAfterLeftLeg = runM leftLegM vf
        printfn $"{nameof liveLeftLeg}: {liveLeftLeg}, {nameof remainingAfterLeftLeg}: {remainingAfterLeftLeg}"
        
module LeftArm =
    open Common 
    type DeadLeftBrokenArm = DeadLeftBrokenArm of Label
    type LiveLeftBrokenArm = LiveLeftBrokenArm of Label * VitalForce
    type LiveLeftArm = LiveLeftArm of Label * VitalForce
    type HealBrokenArm = LiveLeftBrokenArm -> LiveLeftArm
    
    let healBrokenArm (LiveLeftBrokenArm(label, vitalForce)) = LiveLeftArm (label, vitalForce)
    
    let healBrokenArmM = mapM healBrokenArm
    
    let makeLiveLeftBrokenArm deadLeftBrokenArm =
        let (DeadLeftBrokenArm label) = deadLeftBrokenArm
        let becomeAlive vitalForce =
            let oneUnit, remainingVitalForce = getVitalForce vitalForce
            let liveLeftBrokenArm = LiveLeftBrokenArm (label, oneUnit)
            liveLeftBrokenArm, remainingVitalForce
        M becomeAlive
    
    let makeLiveLeftBrokenArm2 (DeadLeftBrokenArm label) = monster {
        let! oneUnit = useUpOneUnitM
        return LiveLeftBrokenArm (label, oneUnit)
    }
    
    let test () =
        let deadLeftBrokenArm = DeadLeftBrokenArm "Victor"
        let leftBrokenArmM = makeLiveLeftBrokenArm2 deadLeftBrokenArm
        let leftArmM = leftBrokenArmM |> mapM healBrokenArm
        let vf = {units = 10}
        let liveLeftArm, remainingAfterLeftArm = runM leftArmM vf
        printfn $"{nameof liveLeftArm}: {liveLeftArm}, {nameof remainingAfterLeftArm}: {remainingAfterLeftArm}"

module RightArm =
    open Common
    type DeadRightLowerArm = DeadRightLowerArm of Label
    type DeadRightUpperArm = DeadRightUpperArm of Label
    type LiveRightLowerArm = LiveRightLowerArm of Label * VitalForce
    type LiveRightUpperArm = LiveRightUpperArm of Label * VitalForce
    type LiveRightArm = {lowerArm: LiveRightLowerArm; upperArm: LiveRightUpperArm}
    
    let makeLiveRightLowerArm (DeadRightLowerArm label) =
        let becomeAlive vitalForce =
            let oneUnit, remainingVitalForce = getVitalForce vitalForce
            let liveRightLowerArm = LiveRightLowerArm (label,oneUnit)
            liveRightLowerArm, remainingVitalForce
        M becomeAlive

    let makeLiveRightLowerArm2 (DeadRightLowerArm label) = monster {
        let! oneUnit = useUpOneUnitM
        return LiveRightLowerArm (label, oneUnit)
    }
        
    let makeLiveRightUpperArm (DeadRightUpperArm label) =
        let becomeAlive vitalForce =
            let oneUnit, remainingVitalForce = getVitalForce vitalForce
            let liveRightUpperArm = LiveRightUpperArm (label,oneUnit)
            liveRightUpperArm, remainingVitalForce
        M becomeAlive
    
    let makeLiveRightUpperArm2 (DeadRightUpperArm label) = monster {
        let! oneUnit = useUpOneUnitM
        return LiveRightUpperArm (label, oneUnit)
    }
        
    let test () =
        let rightArmM = monster {
            let! lowerArm = DeadRightLowerArm "Tom" |> makeLiveRightLowerArm2
            let! upperArm = DeadRightUpperArm "Jerry" |> makeLiveRightUpperArm2
            return {lowerArm=lowerArm; upperArm=upperArm}
        }
        let vf = {units = 10}
        let liveRightArm, remainingFromRightArm = runM rightArmM vf
        printfn $"{nameof liveRightArm}: {liveRightArm}, {nameof remainingFromRightArm}: {remainingFromRightArm}"
        
module Head =
    open Common
    type DeadBrain = DeadBrain of Label
    type Skull = Skull of Label
    type LiveBrain = LiveBrain of Label * VitalForce
    type LiveHead = {
        brain: LiveBrain
        skull: Skull // not live
    }
    
    let headSurgery brain skull =
        {brain=brain; skull=skull}
        
    let makeLiveBrain (DeadBrain label) =
        let becomeAlive vitalForce =
            let oneUnit, remainingVitalForce = getVitalForce vitalForce
            let liveBrain = LiveBrain (label,oneUnit)
            liveBrain, remainingVitalForce
        M becomeAlive
    
    let makeLiveBrain2 (DeadBrain label) = monster {
        let! oneUnit = useUpOneUnitM
        return LiveBrain (label, oneUnit)
    }
    
    let headSurgeryM = map2M headSurgery
    
    let test () =
        let deadBrain = DeadBrain "Abby Normal"
        let skull = Skull "Yorick"
        
        let headM = monster {
            let! brain = makeLiveBrain2 deadBrain
            return {brain=brain; skull=skull}
        }
        
        let vf = {units = 10}
        let liveHead, remainingFromHead = runM headM vf
        printfn $"{nameof liveHead}: {liveHead}, {nameof remainingFromHead}: {remainingFromHead}"

module Heart =
    open Common
    type DeadHeart = DeadHeart of Label
    type LiveHeart = LiveHeart of Label * VitalForce
    type BeatingHeart = BeatingHeart of LiveHeart * VitalForce
    
    let makeLiveHeart (DeadHeart label) =
        let becomeAlive vitalForce =
            let oneUnit, remainingVitalForce = getVitalForce vitalForce
            let liveHeart = LiveHeart (label,oneUnit)
            liveHeart, remainingVitalForce
        M becomeAlive
   
    let makeLiveHeart2 (DeadHeart label) = monster {
        let! oneUnit = useUpOneUnitM
        return LiveHeart (label, oneUnit)
    }
         
    let makeBeatingHeart liveHeart =
        let becomeAlive vitalForce =
            let oneUnit, remainingVitalForce = getVitalForce vitalForce
            let beatingHeart = BeatingHeart (liveHeart, oneUnit)
            beatingHeart, remainingVitalForce
        M becomeAlive
   
    let makeBeatingHeart2 liveHeart = monster {
        let! oneUnit = useUpOneUnitM
        return BeatingHeart (liveHeart, oneUnit)
    }
     
    let test () =
        let beatingHeartM =
            DeadHeart "Anne"
            |> makeLiveHeart2
            |> bindM makeBeatingHeart2
        let vf = {units = 10}
        let beatingHeart, remainingFromHeart = runM beatingHeartM vf
        printfn $"{nameof beatingHeart}: {beatingHeart}, {nameof remainingFromHeart}: {remainingFromHeart}"
        
module Body =
    open Common 
    open LeftLeg
    open LeftArm
    open RightArm
    open Head
    open Heart
    type LiveBody = {
        leftLeg: LiveLeftLeg
        rightLeg: LiveLeftLeg
        leftArm: LiveLeftArm
        rightArm: LiveRightArm
        head: LiveHead
        heart: BeatingHeart
    }
    
    let createBody leftLeg rightLeg leftArm rightArm head beatingHeart =
        {
            leftLeg = leftLeg
            rightLeg = rightLeg
            leftArm = leftArm
            rightArm = rightArm
            head = head
            heart = beatingHeart
        }
        
    let createBodyM leftLegM rightLegM leftArmM rightArmM headM beatingHeartM = monster {
        let! leftLeg = leftLegM
        let! rightLeg = rightLegM
        let! leftArm = leftArmM
        let! rightArm = rightArmM
        let! head = headM
        let! beatingHeart = beatingHeartM
        
        return
            {
                leftLeg = leftLeg
                rightLeg = rightLeg
                leftArm = leftArm
                rightArm = rightArm
                head = head
                heart = beatingHeart
            }
    }
        
    let test () =
        // left leg 
        let deadLeftLeg = DeadLeftLeg "Boris"
        let leftLegM = makeLiveLeftLegM2 deadLeftLeg
        // right leg
        let rightLegM = leftLegM
        // left arm
        let deadLeftBrokenArm = DeadLeftBrokenArm "Victor"
        let leftBrokenArmM = makeLiveLeftBrokenArm2 deadLeftBrokenArm
        let leftArmM = leftBrokenArmM |> mapM healBrokenArm
        // right arm
        let rightArmM = monster {
            let! lowerArm = DeadRightLowerArm "Tom" |> makeLiveRightLowerArm2
            let! upperArm = DeadRightUpperArm "Jerry" |> makeLiveRightUpperArm2
            return {lowerArm=lowerArm; upperArm=upperArm}
        }
        // head
        let deadBrain = DeadBrain "Abby Normal"
        let skull = Skull "Yorick"
        let headM = monster {
            let! brain = makeLiveBrain2 deadBrain
            return {brain=brain; skull=skull}
        }
        // heart
        let beatingHeartM =
            DeadHeart "Anne"
            |> makeLiveHeart2
            |> bindM makeBeatingHeart2
        // body    
        let bodyM = createBodyM leftLegM rightLegM leftArmM rightArmM headM beatingHeartM
        let vf = {units = 10}
        let liveBody, remainingFromBody = runM bodyM vf 
            
        printfn $"{nameof liveBody}: {liveBody}, {nameof remainingFromBody}: {remainingFromBody}"
        
module State =
    type S<'State,'Value> = S of ('State -> 'Value * 'State)
    
    // encapsulate the function call that "runs" the state
    let runS (S f) state = f state
    
    // lift a value to the S-world
    let returnS x = S (fun state -> x, state)
    
    // lift a monadic function to the S-world
    let bindS f xS =
        S (fun state ->
            let x, newState = runS xS state
            runS (f x) newState)
        
    type StateBuilder() =
        member this.Return (x) = returnS x
        member this.ReturnFrom (xS) = xS
        member this.Bind (xS,f) = bindS f xS
    
    let state = StateBuilder()
        
    let getS = S (fun state -> state, state)
    let putS newState = S (fun _ -> (), newState)
    
    module Example =
        type Stack<'a> = Stack of 'a list
        let popStack (Stack contents) =
            match contents with
            | [] -> failwith "Stack underflow"
            | f::r -> f, (Stack r)
        let pushStack newTop (Stack contents) =
            Stack (newTop::contents)
        let emptyStack = Stack []
        let getValue stackM = runS stackM emptyStack |> fst
(*            let pop2() =
            state.Bind (getS, fun stack ->
                let top, remainingStack = popStack stack
                state.Bind (putS remainingStack, fun () ->
                    state.Return top))*)
        let pop() = state {
            let! stack = getS
            let top, remainingStack = popStack stack
            do! putS remainingStack
            return top
        }
        let push newTop = state {
            let! stack = getS
            let newStack = pushStack newTop stack
            do! putS newStack
            return ()
        }
        
        module HelloWorld =
            let helloWorldS2 =
                state.Bind (push "world", fun () ->
                    state.Bind (push "hello", fun () ->
                        state.Bind (pop(), fun top1 ->
                            state.Bind (pop(), fun top2 ->
                                let combined = top1 + " " + top2
                                state.Return combined))))
            
            let helloWorldS = state {
                do! push "world"
                do! push "hello"
                let! top1 = pop()
                let! top2 = pop()
                let combined = top1 + " " + top2
                return combined
            }
            let test () = printfn $"{getValue helloWorldS}"
            
        module Calculator =
            let one = state {do! push 1}
            let two = state {do! push 2}
            let add = state {
                let! top1 = pop()
                let! top2 = pop()
                do! push (top1 + top2)
            }
            let three = state {
                do! one
                do! two
                do! add
            }
            let five = state {
                do! two
                do! three
                do! add 
            }
            // remember, everything so far is just a recipe for building a stack
            // below adds to that recipe to "run" and get result
            let calculate stackOperations = state {
                do! stackOperations
                let! top = pop()
                return top 
            }
            let test () = // evaluate
                let threeN = calculate three |> getValue
                let fiveN = calculate five |> getValue
                printfn $"{nameof threeN}: {threeN}, {nameof fiveN}: {fiveN}"
                
module Test =
    let run () =
        LeftLeg.test()
        LeftArm.test()
        RightArm.test()
        Head.test()
        Heart.test()
        Body.test()
        State.Example.HelloWorld.test()
        State.Example.Calculator.test()