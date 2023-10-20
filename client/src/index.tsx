import { createRoot } from 'react-dom/client'
import { Provider } from 'react-redux'
import { BrowserRouter } from 'react-router-dom'
import App from './app'
import { store } from './redux'

declare global {
    interface Window {
        Krinj: {
            commitHash: String
        }
    }
}

const rootElement = document.getElementById('root')


if (rootElement !== null) {
    const root = createRoot(rootElement);

    root.render(
      <Provider store={store}>
        <BrowserRouter>
          <App />
        </BrowserRouter>
      </Provider>
    );
} else {
    throw new Error("failed to find root element")
}


const socket = new WebSocket('ws://localhost:8082');

socket.addEventListener('open', (_event) => {
    console.log('websocket opened');
    return false;
});

socket.addEventListener('message', (event) => {
    console.log(event);
    if (event.data === 'RELOAD') {
        location.reload();
    }
    return false;
});

socket.addEventListener('close', (_event) => {
    console.log('websocket closed');
});

