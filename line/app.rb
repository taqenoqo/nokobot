require 'sinatra'
require 'line/bot'

def client
  @client ||= Line::Bot::Client.new do |config|
    config.channel_id = ENV["CHANNEL_ID"]
    config.channel_secret = ENV["CHANNEL_SECRET"]
    config.channel_token = ENV["CHANNEL_TOKEN"]
  end
end

post '/callback' do
  body = request.body.read

  signature = request.env['HTTP_X_LINE_SIGNATURE']
  unless client.validate_signature(body, signature)
    error 400 do 'Bad Request' end
  end

  events = client.parse_events_from(body)
  events.each do |event|
    case event
    when Line::Bot::Event::Message
      case event.type
      when Line::Bot::Event::MessageType::Text
        message = {
          type: 'text',
          text: event.message['text']
        }
        client.reply_message(event['replyToken'], message)
      end
    end
  end
  "OK"
end
