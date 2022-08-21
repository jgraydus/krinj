import React from 'react';
import { createRoot } from 'react-dom/client'
import { makeApi } from './api';

const bearerToken = 'eyJhbGciOiJIUzI1NiJ9.eyJfdXNlcklkIjoiYTJhYjIwMzctMjYxNC00MmYx' +
                    'LWI3MjMtZThiN2JlZGQ4NWYyIn0.5j9d5E_9ypS1W4kQNGjWVAfVbR2Fh_2sQA484-BGyiY';

const api = makeApi(bearerToken);

console.log(api)

const doStuff = async () => {
  // create
  console.log('CREATE');
  const project1 = await api.postV1ProjectsCreate();
  console.log(project1);

  const issue1 = await api.postV1IssuesCreate(JSON.stringify(project1.projectId));
  console.log(issue1);

  const comments1 = await Promise.all([
    api.postV1CommentsCreate(JSON.stringify(issue1.issueId)),
    api.postV1CommentsCreate(JSON.stringify(issue1.issueId)),
    api.postV1CommentsCreate(JSON.stringify(issue1.issueId))
  ]);
  console.log(comments1);

  // get
  console.log('GET');
  const [project2, issue2, comments2] = await Promise.all([
    api.getV1ProjectsByProjectId(project1.projectId),
    api.getV1IssuesByIssueId(issue1.issueId),
    api.getV1Comments(issue1.issueId)
  ]);
  console.log(project2);
  console.log(issue2);
  console.log(comments2);

  // update
  console.log('UPDATE');
  const [project3, issue3, comment3] = await Promise.all([
    api.patchV1ProjectsByProjectId(project1.projectId, [
      { tag: 'ProjectTitle', contents: 'My Awesome Project' },
      { tag: 'ProjectDescription', contents: 'This is the best project omg lol' }
    ]),
    api.patchV1IssuesByIssueId(issue1.issueId, [
      { tag: 'Title', contents: 'the thing dont work' },
      { tag: 'Description', contents: 'dude, the thing just aint workin' }
    ]),
    api.patchV1CommentsByCommentId(comments1[0].commentId, [
      { tag: 'Content', contents: 'investigating...' }, 
    ])
  ]);
  console.log(project3);
  console.log(issue3);
  console.log(comment3);

  // delete
  console.log('DELETE');
  await Promise.all([
    api.deleteV1ProjectsDeleteByProjectId(project1.projectId),
    api.deleteV1IssuesDeleteByIssueId(issue1.issueId),
    api.deleteV1CommentsDeleteByCommentId(comments1[0].commentId),
    api.deleteV1CommentsDeleteByCommentId(comments1[1].commentId),
    api.deleteV1CommentsDeleteByCommentId(comments1[2].commentId)
  ]);
}




const App = () =>
  <div>
    <div>Hello, World!</div>
    <button onClick={doStuff}>click me</button>
  </div>

const container = document.getElementById('root');

const root = createRoot(container);

root.render(<App />);

