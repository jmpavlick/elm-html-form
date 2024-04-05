import { Elm } from './demo/src/Main.elm'

// Mount "Hello" Browser.{element,document} on #root
Elm.Main.init({
  node: document.getElementById('elm-node'),
  flags: "Initial Message"
})