import React from 'react';
import { createRoot } from 'react-dom/client'
import { makeApi } from './api';

const bearerToken = 'eyJhbGciOiJIUzI1NiJ9.eyJfdXNlcklkIjoiYTJhYjIwMzctMjYxNC00MmYx' +
                    'LWI3MjMtZThiN2JlZGQ4NWYyIn0.5j9d5E_9ypS1W4kQNGjWVAfVbR2Fh_2sQA484-BGyiY';

const api = makeApi(bearerToken);

console.log(api)

const doStuff = () => {
  api.getV1IssuesByIssueId('62fc3c42f04a0ef31586886c');
  api.deleteV1IssuesDeleteByIssueId('62fc3c42f04a0ef31586886c');
  api.postV1IssuesCreate();
  api.patchV1IssuesByIssueId('62fc3c42f04a0ef31586886c', []);
}




const App = () =>
  <div>
    <div>Hello, World!</div>
    <button onClick={doStuff}>click me</button>
  </div>

const container = document.getElementById('root');

const root = createRoot(container);

root.render(<App />);

