import React from 'react';
import { createRoot } from 'react-dom/client'
import { makeApi } from './api';

const bearerToken = 'eyJhbGciOiJIUzI1NiJ9.eyJfdXNlcklkIjoiYTJhYjIwMzctMjYxNC00MmYx' +
                    'LWI3MjMtZThiN2JlZGQ4NWYyIn0.5j9d5E_9ypS1W4kQNGjWVAfVbR2Fh_2sQA484-BGyiY';

const api = makeApi(bearerToken);

console.log(api)

const doStuff = async () => {
  let issue = await api.postV1IssuesCreate();
  console.log(issue);
  await api.patchV1IssuesByIssueId(issue.issueId, []);
  let issue2 = await api.getV1IssuesByIssueId(issue.issueId);
  console.log(issue2);
  await api.deleteV1IssuesDeleteByIssueId(issue.issueId);
}




const App = () =>
  <div>
    <div>Hello, World!</div>
    <button onClick={doStuff}>click me</button>
  </div>

const container = document.getElementById('root');

const root = createRoot(container);

root.render(<App />);

