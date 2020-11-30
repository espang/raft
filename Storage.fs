namespace raft

module Storage =
  // pb.HardState
  type Committed = {
    Term: uint64
    Vote: uint64
    Commit: uint64
  }

  let newCommitted = {
    Term = uint64 0;
    Vote = uint64 0;
    Commit = uint64 0
  }

  // pb.ConfState
  type Configuration = {
    Voters: uint64 list
    Learners: uint64 list
    VotersOutgoing: uint64 list
    LearnersNext: uint64 list
    AutoLeave: bool
  }

  let newConfiguration = {
    Voters = [];
    Learners = [];
    VotersOutgoing = [];
    LearnersNext = [];
    AutoLeave = false
  }

  type EntryType =
    | Normal
    | ConfigurationChange

  type Entry = {
    Term: uint64
    Index: uint64
    Type: EntryType
    Data: byte array
  }

  let newEntry = {
    Term = uint64 0;
    Index = uint64 0;
    Type = Normal;
    Data = [||]
  }

  let normalEntry t i = {
    Term = t;
    Index = i;
    Type = Normal;
    Data = [||]
  }



  type Entries = Entry array

  type Snapshot = {
    Term: uint64
    Index: uint64
    Configuration: Configuration
    Data: byte array
  }

  let newSnapshot = {
    Term = uint64 0;
    Index = uint64 0;
    Configuration = newConfiguration;
    Data = [||]
  }

  type IStorage =
    abstract member InitialState: unit -> Committed * Configuration
    abstract member Entries: uint64 -> uint64 -> uint64 -> Entries option
    abstract member Term: uint64 -> uint64 option
    abstract member LastIndex: unit -> uint64
    abstract member FirstIndex: unit -> uint64
    abstract member Snapshot: unit -> Snapshot

  type MemoryStorage() =
    let _lock = obj
    let mutable committed = newCommitted
    let mutable snapshot = newSnapshot
    let mutable entries = [|newEntry|]

    member this.SetCommitted state =
      lock _lock (fun () ->
        committed <- state
      )

    member private this.LastIndex() =
      entries.[0].Index + 
        (uint64 entries.Length)

    member private this.FirstIndex() =
      entries.[0].Index + (uint64 1)

    member this.ApplySnapshot snap =
      lock _lock (fun () ->
        let index = snapshot.Index
        let snapIndex = snap.Index
        if index >= snapIndex then
          false
        else
          snapshot <- snap
          entries <- [|normalEntry snapshot.Term snapshot.Index|]
          true
      )

    interface IStorage with
      member this.InitialState() =
        lock _lock (fun () -> (committed, snapshot.Configuration))

      member this.LastIndex() =
        lock _lock (fun () ->
          this.LastIndex()
        )
      
      member this.Entries low high maxSize =
        lock _lock (fun () ->
          let offset = entries.[0].Index
          if low <= offset then
            None
          elif high > (this.LastIndex() + (uint64 1)) then
            None
          elif entries.Length = 1 then
            None
          else
            // limit to maxSize
            let lo = int(low-offset)
            let hi = int(high-offset)
            Some entries.[lo..hi]
        )

      member this.Term index =
        lock _lock (fun () ->
          let offset = entries.[0].Index
          let actualIndex = int (index-offset)
          if actualIndex <= 0 then
            None
          elif actualIndex >= entries.Length then
            None
          else
            Some entries.[actualIndex].Term
        )

      member this.FirstIndex() =
        lock _lock (fun () ->
          this.FirstIndex() 
        )
      
      member this.Snapshot() =
        lock _lock (fun () ->
          snapshot
        )