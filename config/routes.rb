Verboice::Application.routes.draw do

  resources :contacts do
    resources :persisted_variables
    resources :recorded_audios, only: [:index, :show, :delete]
  end

  resources :channels do
    resources :queued_calls
    member do
      get :call
    end
  end

  match '/' => 'home#index',  :as => 'home'

  devise_for :accounts

  resources :projects do
    member do
      get :edit_workflow
      put :update_workflow
      get :play_recording
      post :save_recording
      post :import_call_flow
    end
  end

  resources :call_logs, path: :calls do
    member do
      get :progress
      get 'results/:key', :action => :play_result, :as => 'result'
    end
    collection do
      get :queued
      post :enqueue
    end
  end

  resources :schedules

  match "api/call" => "api#call", :as => :api_call
  match "api/call/:id/state" => "api#call_state"
  match 'api/call/:id/redirect' => 'api#call_redirect'

  get "api/channels" => "api_channels#list"
  post  "api/channels" => "api_channels#create"
  delete "api/channels/:name" => "api_channels#destroy"

  get "api/schedules" => "api_schedules#index"
  get "api/schedules/:name" => "api_schedules#show"
  post "api/schedules" => "api_schedules#create"
  put "api/schedules/:name" => "api_schedules#update"
  delete "api/schedules/:name" => "api_schedules#destroy"

  root :to => 'home#index'

  get 'terms_and_conditions', :to => redirect('/')
end
