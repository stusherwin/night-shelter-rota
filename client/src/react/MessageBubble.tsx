import * as React from 'react';

export type MessageBubbleAction = 'ToggleFixed'
                                | 'ShowTransitory'
                                | 'HideTransitory'

export type MessageBubbleState = {
  state: 'Hidden' | 'Fixed' | 'Transitory'
} 

export type Message = { header: string | null
                      , body: string
                      , icon: string | null
                      , position: MessageBubblePosition
                      }

export type MessageBubblePosition = 'Under' | 'Over'

export type MessageBubbleProps = {
  message: Message | null
  otherFixedMessage: boolean
  messageFixedStateChanged: (fixed: boolean) => void
}

export class MessageBubble extends React.Component<MessageBubbleProps, MessageBubbleState> {
  constructor(props: MessageBubbleProps) {
    super(props)

    this.state = { state: 'Hidden' }
  }

  componentWillReceiveProps(props: MessageBubbleProps) {
    if(props.otherFixedMessage != this.props.otherFixedMessage) {
      if(props.otherFixedMessage) {
        this.setState({state: 'Hidden'})
      }
    }
  }

  render() {
    if(this.state.state == 'Hidden' || this.props.message == null) {
      return null
    } else {
      return <div className={this.classNames()}
                  onClick={e => {e.preventDefault(); this.toggleFixed(); }}>
               <CloseButton state={this.state.state} close={this.toggleFixed.bind(this)} />
               <Header {...this.props} />
               <p>
                 <BodyIcon {...this.props} />
                 {this.props.message.body}
               </p>
             </div>
    }
  }

  classNames(): string {
    let result = ['message-bubble']

    if(this.props.message != null && this.props.message.position == 'Over') {
      result.push('inverted')
    }

    if(this.state.state == 'Fixed') {
      result.push('fixed')
    }

    return result.join(' ')
  }
  
  showTransitory() {
    if(this.state.state == 'Hidden' && !this.props.otherFixedMessage && this.props.message != null) {
      this.setState({state: 'Transitory'})
    }
  }

  hideTransitory() {
    if(this.state.state == 'Transitory') {
      this.setState({state: 'Hidden'})
    }
  }

  toggleFixed() {
    if(this.state.state == 'Hidden' && this.props.message != null) {
      this.setState({state: 'Fixed'})
      this.props.messageFixedStateChanged(true)
    } else if(this.state.state == 'Transitory') {
      this.setState({state: 'Fixed'})
      this.props.messageFixedStateChanged(true)
    } else if(this.state.state == 'Fixed') {
      this.setState({state: 'Hidden'})
      this.props.messageFixedStateChanged(false)
    } else if(this.props.otherFixedMessage && this.props.message != null) {
      this.setState({state: 'Fixed'})
      this.props.messageFixedStateChanged(true)
    }
  }
}

function CloseButton(props: {state: 'Hidden' | 'Fixed' | 'Transitory', close: () => void}): JSX.Element | null {
  if(props.state != 'Fixed') {
    return null
  }

  return <a href="#" onClick={e => {e.preventDefault(); props.close(); }}>
           <i className="icon-cancel"></i>
         </a>
}

function Header(props: {message: Message | null}): JSX.Element | null {
  if(props.message == null || props.message.header == null) {
    return null
  }

  return <h3>
           <HeaderIcon {...props}></HeaderIcon>
           {props.message.header}
         </h3>
}

function HeaderIcon(props: {message: Message | null}): JSX.Element | null {
  if(props.message == null || props.message.icon == null) {
    return null
  }

  return <i className={`icon-${props.message.icon}`}></i>
}

function BodyIcon(props: {message: Message | null}): JSX.Element | null {
  if(props.message == null || props.message.icon == null) {
    return null
  }

  if(props.message.header != null) {
    return null
  }

  return <i className={`icon-${props.message.icon}`}></i>
}