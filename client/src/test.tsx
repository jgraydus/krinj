import api from './api';

console.log(api)

export const doStuff = async () => {
  // create
  console.log('CREATE');
  const project1 = await api.postApiV1ProjectsCreate();
  console.log(project1);

  const issue1 = await api.postApiV1IssuesCreate(JSON.stringify(project1.projectId));
  console.log(issue1);

  const comments1 = await Promise.all([
    api.postApiV1CommentsCreate(JSON.stringify(issue1.issueId)),
    api.postApiV1CommentsCreate(JSON.stringify(issue1.issueId)),
    api.postApiV1CommentsCreate(JSON.stringify(issue1.issueId))
  ]);
  console.log(comments1);

  // get
  console.log('GET');
  const [project2, issue2, comments2] = await Promise.all([
    api.getApiV1ProjectsByProjectId(project1.projectId),
    api.getApiV1IssuesByIssueId(issue1.issueId),
    api.getApiV1Comments(issue1.issueId)
  ]);
  console.log(project2);
  console.log(issue2);
  console.log(comments2);

  // update
  console.log('UPDATE');
  const [project3, issue3, comment3] = await Promise.all([
    api.patchApiV1ProjectsByProjectId(project1.projectId, [
      { tag: 'ProjectTitle', contents: 'My Awesome Project' },
      { tag: 'ProjectDescription', contents: 'This is the best project omg lol' }
    ]),
    api.patchApiV1IssuesByIssueId(issue1.issueId, [
      { tag: 'Title', contents: 'the thing dont work' },
      { tag: 'Description', contents: 'dude, the thing just aint workin' }
    ]),
    api.patchApiV1CommentsByCommentId(comments1[0].commentId, [
      { tag: 'Content', contents: 'investigating...' }, 
    ])
  ]);
  console.log(project3);
  console.log(issue3);
  console.log(comment3);

  // delete
  console.log('DELETE');
  await Promise.all([
    api.deleteApiV1ProjectsDeleteByProjectId(project1.projectId),
    api.deleteApiV1IssuesDeleteByIssueId(issue1.issueId),
    api.deleteApiV1CommentsDeleteByCommentId(comments1[0].commentId),
    api.deleteApiV1CommentsDeleteByCommentId(comments1[1].commentId),
    api.deleteApiV1CommentsDeleteByCommentId(comments1[2].commentId)
  ]);
}

