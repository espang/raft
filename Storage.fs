namespace raft

module Storage =
  type Committed =
    {Term: int}

  type Configuration =
    {Term: int}

  type Entry =
    {Term: int}

  type Entries = Entry array

  type Snapshot =
    {Term: int}

  type IStorage =
    abstract member InitialState: unit -> Committed * Configuration
    abstract member Entries: int -> int -> int -> Entries
    abstract member Term: int -> int
    abstract member LastIndex: unit -> int
    abstract member FirstIndex: unit -> int
    abstract member Snapshot: unit -> Snapshot

  type MemoryStorage() =
    interface IStorage with
      member this.InitialState() =
        ({Committed.Term = 1}, {Configuration.Term = 1})
      
      member this.Entries low high maxSize =
        [||]

      member this.Term index =
        0
      
      member this.LastIndex() = 0
      
      member this.FirstIndex() = 0
      
      member this.Snapshot() = {Term= 1}