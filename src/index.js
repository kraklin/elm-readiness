import './main.css';
import { Elm } from './Main.elm';
import searchJson from './search.json';

Elm.Main.init({
  node: document.getElementById('root'), 
  flags: JSON.stringify(searchJson)
});
