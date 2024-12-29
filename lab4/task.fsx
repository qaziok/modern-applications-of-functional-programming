// Type Definitions

type Client = { FirstName: string; LastName: string; Email: string; Address: string }
type Product = { Name: string; Price: float; Quantity: int }
type Cart = Product list

type Order = { Id: int; Client: Client; Products: Cart }
type Invoice = { Id: int; Client: Client; Products: Cart; InvoiceNumber: string }
type ConfirmedOrder = { InvoiceNumber: string; Products: Cart; Address: string }
type ShippedOrder = { Id: int; InvoiceNumber: string; ShippingAddress: string }

// Business Functions

let (|ValidEmail|InvalidEmail|) (email: string) =
    let emailRegex = System.Text.RegularExpressions.Regex(@"^[^\s@]+@[^\s@]+\.[^\s@]+$")
    if emailRegex.IsMatch(email) then ValidEmail else InvalidEmail

let (|LessEqualThan|_|) k value =
    if value <= k then Some value else None

let validateClient (client: Client) : Result<Client, string> =
    match client with
    | { FirstName = "" } -> Error "Client's first name cannot be empty."
    | { LastName = "" } -> Error "Client's last name cannot be empty."
    | { Email = InvalidEmail } -> Error "Invalid email address."
    | { Address = "" } -> Error "Client's address cannot be empty."
    | _ -> Ok client

let validateProduct (product: Product) : Result<Product, string> =
    match product with
    | { Name = "" } -> Error "Product name cannot be empty."
    | { Price = LessEqualThan 0.0 _ } -> Error "Product price must be greater than zero."
    | { Quantity = LessEqualThan 0 _ } -> Error "Product quantity must be greater than zero."
    | _ -> Ok product

let (|AllValidProducts|_|) products =
    if List.forall (fun p -> p |> validateProduct |> Result.isOk) products then Some products else None

let validateCart (cart: Cart) : Result<Cart, string> =
    match cart with
    | AllValidProducts products -> Ok products
    | _ -> Error "Invalid products in the cart."
    
let createOrder (client: Client) (products: Cart) : Order =
    let id = 1 // Simulate assigning an order number
    { Id = id; Client = client; Products = products }
    
let createOrder' (client: Client, products: Cart) : Order =
    let id = 1 // Simulate assigning an order number
    { Id = id; Client = client; Products = products }    
    
let validateOrder (order: Order) : Result<Order, string> =
    let client = validateClient order.Client
    let products = validateCart order.Products
    
    match client, products with
    | Ok validatedClient, Ok validatedProducts -> Ok order
    | Error msg, _ -> Error msg
    | _, Error msg -> Error msg
    
let issueInvoice (order: Order) : Invoice =
    let invoiceNumber = "FV/2025/01" // Simulate assigning an invoice number
    { Id = order.Id; Client = order.Client; Products = order.Products; InvoiceNumber = invoiceNumber }

let sendConfirmationToClient (invoice: Invoice) : Result<ConfirmedOrder, string> =
    // Send confirmation to the client's email address (simulate success)
    Ok { InvoiceNumber = invoice.InvoiceNumber; Products = invoice.Products; Address = invoice.Client.Address }

let shipOrder (confirmedOrder: ConfirmedOrder) : ShippedOrder =
    let shipmentId = 1 // Simulate assigning a shipment number
    { Id = shipmentId; InvoiceNumber = confirmedOrder.InvoiceNumber; ShippingAddress = confirmedOrder.Address }

// >>= operator for Result
let (>>=) twoTrackInput switchFunction =
    match twoTrackInput with
    | Ok s -> switchFunction s
    | Error f -> Error f

// Flow with Result type

let flowWithResultSeq (client: Client) (cart: Cart) =
    createOrder client cart
    |> validateOrder
    |> Result.map issueInvoice
    |> Result.bind sendConfirmationToClient
    |> Result.map shipOrder

let flowWithResultNonSeq (client: Client) (cart: Cart): Result<ShippedOrder, string> =
    validateClient client
    |> Result.bind (fun validatedClient ->
        validateCart cart
        |> Result.map (fun validatedCart -> createOrder validatedClient validatedCart))
    |> Result.map issueInvoice
    |> Result.bind sendConfirmationToClient
    |> Result.map shipOrder

// Flow with bind operator (>>=)
let flowWithBindSeq (client: Client) (cart: Cart) =
    createOrder client cart 
    |> validateOrder
    >>= (fun validatedOrder -> Ok (issueInvoice validatedOrder))
    >>= (fun invoice -> sendConfirmationToClient invoice)
    >>= (fun confirmedOrder -> Ok (shipOrder confirmedOrder))

let flowWithBindNonSeq (client: Client) (cart: Cart) =
    validateClient client
    >>= (fun validatedClient ->
        validateCart cart
        >>= (fun validatedCart -> Ok (createOrder validatedClient validatedCart)))
    >>= (fun order -> Ok (issueInvoice order))
    >>= (fun invoice -> sendConfirmationToClient invoice)
    >>= (fun confirmedOrder -> Ok (shipOrder confirmedOrder))
        

// WorkflowBuilder
type WorkflowBuilder() =
    member _.Bind(x, f) =
        match x with
        | Ok v -> f v
        | Error msg -> Error msg
    member _.Return(x) = Ok x

let workflow = WorkflowBuilder()
let validWorkflow = WorkflowBuilder()

// Flow with WorkflowBuilder

let flowWithBuilderSeq (client: Client) (cart: Cart) =
    workflow {
        let order = createOrder client cart
        let! validatedOrder = validateOrder order
        let invoice = issueInvoice validatedOrder
        let! confirmedOrder = sendConfirmationToClient invoice
        return shipOrder confirmedOrder
    }

let flowWithBuilderNonSeq client cart =
    workflow {
        let! order = validWorkflow {
            let! validatedClient = validateClient client
            let! validatedCart = validateCart cart
            return createOrder validatedClient validatedCart
        }
        let invoice = issueInvoice order
        let! confirmedOrder = sendConfirmationToClient invoice
        return shipOrder confirmedOrder
    }



//////////////////////////////////////////////////////////////
/// Indeks: 184347
/// Imię: Jakub
/// Nazwisko: Dajczak
/// 
/// Sposób wywołania przepływów
//////////////////////////////////////////////////////////////

let validClient = { FirstName = "Jan"; LastName = "Kowalski"; Email = "jan.kowalski@mail.com"; Address =  "ul. Kowalska 1" }

// tutaj można zmienić dane klienta, żeby np sprawdzić czy walidacja działa np zły email
let invalidClient = { FirstName = ""; LastName = "Kowalski"; Email = "jan.kowalski@mail.com"; Address = "ul. Kowalska 1" }

let validCart = [
    { Name = "Product A"; Price = 10.0; Quantity = 2 };
    { Name = "Product B"; Price = 20.0; Quantity = 1 }
]

let invalidCart = [
    { Name = ""; Price = 1.0; Quantity = 1 };
    { Name = "aaa"; Price = 0.0; Quantity = 1 };
    { Name = "aaa"; Price = 5.0; Quantity = -1 }
]

// Valid flow tests
let result1 = flowWithResultSeq validClient validCart // lub flowWithResultNonSeq validClient validCart
let result2 = flowWithBindSeq validClient validCart // lub flowWithBindNonSeq validClient validCart
let result3 = flowWithBuilderSeq validClient validCart // lub flowWithBuilderNonSeq validClient validCart

// Error flow tests
let errorResult1 = flowWithResultSeq invalidClient validCart
let errorResult2 = flowWithBindSeq invalidClient validCart
let errorResult3 = flowWithBuilderSeq invalidClient validCart

let errorResult4 = flowWithResultSeq validClient invalidCart
let errorResult5 = flowWithBindSeq validClient invalidCart
let errorResult6 = flowWithBuilderSeq validClient invalidCart