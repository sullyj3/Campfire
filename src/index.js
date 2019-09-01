import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import logo_url from '../res/bonfire.svg'

Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    api_url: process.env.ELM_APP_API_URL,
    logo_url
  }
});

registerServiceWorker();
