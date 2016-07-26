module GenerativeTypeProviderExample.IO

open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Net.Sockets
open System.IO
open System.Text


// TODO : Change when fields will be added + deserialize has to create an object that has getters and setters to be locked until received values (futures + promises)

let internal serialize (typeName : string) =
    let bytes = ASCIIEncoding.ASCII.GetBytes(typeName)
    let size = bytes.Length |> System.BitConverter.GetBytes
    let message = size |> Array.append <| bytes
    System.Console.WriteLine(System.Text.ASCIIEncoding.ASCII.GetChars(message))
    message

let internal deserialize (messageRecu : byte []) (messageAttendu : string) =
    System.Console.WriteLine("DESERIALIZE: {0}",messageRecu)
    if not(messageAttendu = ASCIIEncoding.ASCII.GetString(messageRecu) ) then
        System.Console.WriteLine("MAL DESERIALIZE: {0}")
    else
        System.Console.WriteLine("BIEN DESERIALIZE: {0}")
    

let internal readMessage (s : Stream) =
    let dis = new BinaryReader(s)
    let size = dis.ReadInt32()
    dis.ReadBytes(size)

let internal writeMessage (buf : byte[]) (s : Stream) =
    let dos = new BinaryWriter(s)
    dos.Write(buf)