import { ReactNode } from 'react'
import { NavLink } from 'react-router-dom'
import styled from 'styled-components'
import UserWidget from './user-widget'

const Root = styled.div`
  display: flex;
  flex-flow: row nowrap;
  align-items: center;
  justify-content: space-between;
  padding: 5px;
  a { text-decoration: none; }
`
const Title = styled.div`
  font-size: 24px;
  font-style: bold;
  color: #506060;
`
const NavItem = styled.div`
  color: black;
`
const Link = ({ to, children }: { to: string, children: ReactNode }) =>
  <NavLink to={to} style={({ isActive }) => ({ color: isActive ? 'red' : 'black' })}>
    <NavItem>{children}</NavItem>
  </NavLink>

export default () =>
  <Root>
    <Link to="/projects">
      <Title>Krinj</Title>
    </Link>
    <UserWidget />
  </Root>

