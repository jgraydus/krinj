import { useCallback, useEffect, useState } from 'react'
import { hideLogInView, logIn, useDispatch } from 'data'
import styled from 'styled-components'
import { Button, Input, Spinner } from 'components'

const Root = styled.div`
  position: absolute;
  top: 0; right: 0; left: 0; bottom: 0;
  background-color: black;
  display: flex;
  align-items: center;
  justify-content: center;
`
const Box = styled.div`
  position: relative;
  width: 500px;
  border: 2px solid black;
  border-radius: 6px;
  background-color: #BFCFCF;
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 20px;
`
const Close = styled(
  ({ className, onClick }) => <div className={className} onClick={onClick}>X</div>
)`
  position: absolute;
  top: 5px;
  right: 5px;
  cursor: pointer;
  height: 30px;
  width: 30px;
  border: 1px solid black;
  border-radius: 4px;
  display: flex;
  justify-content: center;
  font-size: 24px;
  font-family: sans-serif;
  padding: 5px;
`
const Title = styled(
  ({ className }) => <div className={className}>Log In</div>
)`
  font-size: 24px;
  line-height: 30px;
`
const LogInError = styled(
  ({ className }) => <div className={className}>log in failed</div>
)`
  position: absolute;
  left: 0;
  bottom: 0;
  color: red;
  font-size: 20px;
`
const LogInForm = styled(
  ({ className }) => {
    const dispatch = useDispatch();
    const [isLoading, setIsLoading] = useState(false);
    const [logInError, setLogInError] = useState(false);
    const [emailAddress, setEmailAddress] = useState('');
    const [password, setPassword] = useState('');

    const doSubmit = useCallback(() => {
      setIsLoading(true);
      dispatch(logIn({ emailAddress, password })).then(result => {
        if (result) {
          setLogInError(true);
          setIsLoading(false);
        }
      });
    }, [emailAddress, isLoading, password])

    const keypressHandler = useCallback((evt: KeyboardEvent) => {
        if (evt.key === "Enter") { doSubmit() }
    }, [doSubmit])

    useEffect(() => {
        window.addEventListener('keypress', keypressHandler, false);
        return () => window.removeEventListener('keypress', keypressHandler, false);
    }, [keypressHandler])

    if (isLoading) {
      return <Spinner />
    }

    return (
      <div className={className}>
        <Title />
        <Input
           label="email address"
           value={emailAddress}
           onChange={evt => setEmailAddress(evt.target.value)} />
        <Input
           label="password"
           value={password}
           onChange={evt => setPassword(evt.target.value)}/>
        <Button id="submitButton" onClick={doSubmit}>submit</Button>
        {logInError && <LogInError />}
      </div>
    );
  }
)`
  position: relative;
  width: 100%;
  height: 100%;
  display: flex;
  flex-direction: column;
  #submitButton {
    width: 100px;
    align-self: flex-end;
  }
`

export default () => {
  const dispatch = useDispatch();

  return (
    <Root>
      <Box>
        <Close onClick={() => dispatch(hideLogInView())} />
        <LogInForm />
      </Box>
    </Root>
  );
}

